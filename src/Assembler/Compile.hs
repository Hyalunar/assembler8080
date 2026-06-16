{-# LANGUAGE LambdaCase #-}

module Assembler.Compile (display) where

import Assembler.Instruction (Instruction)
import qualified Assembler.Instruction as Instruction
import Control.Arrow ((&&&))
import Control.Category ((>>>))
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as ByteString.Lazy
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word8)

display :: (Functor f) => f Instruction -> Map Text Word8 -> f (Text, Instruction)
display insts labels = (instHex &&& id) <$> insts
 where
  instHex inst =
    const inst
      >>> Instruction.assemble lookupLabel
      >>> Builder.toLazyByteString
      >>> ByteString.Lazy.foldl (\l w -> l <> hex w) mempty
      >>> Text.justifyLeft 4 ' '
      $ ()
  lookupLabel lblName = labels Map.! lblName

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
