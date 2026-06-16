{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Assembler.Compile (display, collectLabels, image, csv) where

import Assembler.Instruction (Instruction)
import qualified Assembler.Instruction as Instruction
import Assembler.Parser (Decl (..))
import Control.Arrow ((&&&), (***))
import Control.Category ((>>>))
import Control.Monad (foldM, foldM_)
import Control.Monad.ST (runST)
import Data.Array.Base (freezeSTUArray)
import qualified Data.Array.Base as MArray
import Data.Array.Unboxed (UArray)
import qualified Data.Array.Unboxed as Array
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.Foldable as Foldable
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word8)

display :: (Foldable f) => f Decl -> [(Text, Instruction)]
display insts = (instHex &&& id) <$> mapMaybe declInst (Foldable.toList insts)
 where
  labels = collectLabels insts
  instHex inst =
    const inst
      >>> Instruction.assemble lookupLabel
      >>> Builder.toLazyByteString
      >>> ByteString.Lazy.foldl (\l w -> l <> hex w) mempty
      >>> Text.justifyLeft 4 ' '
      $ ()
  lookupLabel lblName = labels Map.! lblName

  declInst :: Decl -> Maybe Instruction
  declInst = \case
    DeclInst inst -> Just inst
    _ -> Nothing

hex :: Word8 -> Text
hex w = Text.singleton (h wlo) <> Text.singleton (h whi)
 where
  (wlo, whi) = w `divMod` 16
  h = \case
    0x0 -> '0'
    0x1 -> '1'
    0x2 -> '2'
    0x3 -> '3'
    0x4 -> '4'
    0x5 -> '5'
    0x6 -> '6'
    0x7 -> '7'
    0x8 -> '8'
    0x9 -> '9'
    0xA -> 'A'
    0xB -> 'B'
    0xC -> 'C'
    0xD -> 'D'
    0xE -> 'E'
    0xF -> 'F'
    _ -> error "hex.h: Impossible! Digit is not in [0; 15]"

collectLabels :: (Foldable f) => f Decl -> Map Text Word8
collectLabels = snd . foldl' step (0, Map.empty)
 where
  step (off, labels) = \case
    DeclOffset newO -> (newO, labels)
    DeclLabel name -> (off, Map.insert name off labels)
    DeclInst i -> (off + Instruction.size i, labels)
    DeclBytes bs -> (off + fromIntegral (ByteString.length bs), labels)

image :: (Foldable f) => f Decl -> (UArray Word8 Word8, UArray Word8 Bool)
image decls = runST $ do
  rom <- MArray.newArray (minBound, maxBound) 0x00
  isSet <- MArray.newArray (minBound, maxBound) False

  let consumeDecl off = \case
        DeclOffset newOff -> pure newOff
        DeclLabel _ -> pure off
        DeclBytes bs -> do
          foldM writeByte off $ ByteString.unpack bs
        DeclInst inst ->
          let
            bytes = ByteString.Lazy.unpack . Builder.toLazyByteString . Instruction.assemble labelLookup $ inst
           in
            foldM writeByte off bytes
       where
        writeByte pos byte = do
          MArray.writeArray rom pos byte
          MArray.writeArray isSet pos True
          pure $ succ pos
  foldM_ consumeDecl 0 decls
  rom' <- freezeSTUArray rom
  isSet' <- freezeSTUArray isSet
  pure (rom', isSet')
 where
  labelLookup = (collectLabels decls Map.!)

csv :: (Foldable f) => f Decl -> Text
csv =
  image
    >>> (Array.elems *** Array.elems)
    >>> uncurry zip
    >>> fmap (\(b, set) -> if set then hex b else Text.empty)
    >>> Text.intercalate ","
