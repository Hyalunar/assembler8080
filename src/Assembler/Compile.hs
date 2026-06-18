{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE Rank2Types #-}

module Assembler.Compile (collectLabels, image, csv, uncsv) where

import Assembler.Instruction (ConstRef (KnownConst), resolveConstRef)
import qualified Assembler.Instruction as Instruction
import Assembler.Parser (Decl (..))
import Control.Arrow ((***))
import Control.Category ((>>>))
import Control.Monad (foldM, foldM_, unless)
import Control.Monad.Except (MonadError (throwError), liftEither, runExcept, runExceptT)
import Control.Monad.ST (ST, runST)
import Control.Monad.Trans (lift)
import Data.Array.Base (STUArray, freezeSTUArray)
import qualified Data.Array.Base as MArray
import Data.Array.Unboxed (UArray)
import qualified Data.Array.Unboxed as Array
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as ByteString.Lazy
import Data.List (genericLength)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word8)
import qualified Data.Word8 as Word8

collectLabels :: (Foldable f) => f Decl -> Map Text Word8
collectLabels = snd . foldl' step (0, Map.empty)
 where
  step (off, labels) = \case
    DeclOffset newO -> (newO, labels)
    DeclLabel name -> (off, Map.insert name off labels)
    DeclInst i -> (off + Instruction.size i, labels)
    DeclBytes bs -> (off + genericLength bs, labels)

writeSTUArray :: (MArray.MArray (STUArray s) e (ST s), Array.Ix i) => STUArray s i e -> i -> e -> ST s ()
writeSTUArray = MArray.writeArray

image :: (Foldable f) => f Decl -> Either Text (UArray Word8 Word8, UArray Word8 Bool)
image decls = runST $ runExceptT $ do
  rom <- lift $ MArray.newArray (minBound, maxBound) 0x00
  isSet <- lift $ MArray.newArray (minBound, maxBound) False

  let
    consumeDecl off = \case
      DeclOffset newOff -> pure newOff
      DeclLabel _ -> pure off
      DeclBytes cs -> do
        word8s <- liftEither $ mapM (resolveConstRef labelLookup) cs
        foldM writeByte off $ word8s
      DeclInst inst -> do
        bytes <- ByteString.Lazy.unpack . Builder.toLazyByteString <$> liftEither (Instruction.assemble labelLookup inst)
        foldM writeByte off bytes
     where
      writeByte pos byte = do
        lift $ writeSTUArray rom pos byte
        lift $ MArray.writeArray isSet pos True
        pure $ succ pos
  foldM_ consumeDecl 0 decls
  rom' <- lift $ freezeSTUArray rom
  isSet' <- lift $ freezeSTUArray isSet
  pure (rom', isSet')
 where
  labelLookup = (collectLabels decls Map.!?)

csv :: (Foldable f) => f Decl -> Either Text Text
csv decls = do
  img <- image decls
  pure
    $ const img
      >>> (Array.elems *** Array.elems)
      >>> uncurry zip
      >>> fmap (\(b, set) -> if set then Word8.hex b else Text.empty)
      >>> Text.intercalate ","
    $ ()

uncsv :: Text -> Either Text [Decl]
uncsv src = runExcept $ do
  let commas = Text.count "," src
  unless (commas == 255) $ do
    throwError $ "csv does not have the expected format: expected 256 cells but got " <> Text.show (1 + commas)
  let
    cell t = if Text.null t then pure Nothing else (liftEither . fmap Just . Word8.unhex) t
  bytes <- mapM cell $ Text.splitOn "," src
  let contigousBytes = byteRanges bytes
  pure $ concatMap parseDecls contigousBytes
 where
  byteRanges = go 0
   where
    splitJusts [] = ([], [])
    splitJusts xs@(Nothing : _) = ([], xs)
    splitJusts (Just x : rest) =
      let
        (justs, nothings) = splitJusts rest
       in
        (x : justs, nothings)

    go :: Word8 -> [Maybe Word8] -> [(Word8, [Word8])]
    go _ [] = []
    go p (Nothing : rest) = go (succ p) rest
    go p (Just w : rest) =
      let
        (cont, rest') = splitJusts rest
       in
        (p, w : cont) : go (succ p + genericLength cont) rest'

  parseDecls :: (Word8, [Word8]) -> [Decl]
  parseDecls (offset, word8s) =
    DeclOffset offset
      : parseBytes word8s
   where
    parseBytes bytes
      | Just (inst, rest) <- Instruction.disassemble bytes =
          DeclInst inst : parseBytes rest
      | byte : rest <- bytes =
          DeclBytes [KnownConst byte] : parseBytes rest
      | [] <- bytes = []
