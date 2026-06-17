{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Word8 (hex, unhex, prettyHex) where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Lazy.Builder (LazyTextBuilder)
import qualified Data.Text.Lazy.Builder as LazyTextBuilder
import Data.Word (Word8)

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

unhex :: Text -> Either Text Word8
unhex (hi Text.:< lo Text.:< Text.Empty) = do
  dhi <- d hi
  dlo <- d lo
  Right $ dhi * 16 + dlo
 where
  d :: Char -> Either Text Word8
  d = \case
    '0' -> Right 0x0
    '1' -> Right 0x1
    '2' -> Right 0x2
    '3' -> Right 0x3
    '4' -> Right 0x4
    '5' -> Right 0x5
    '6' -> Right 0x6
    '7' -> Right 0x7
    '8' -> Right 0x8
    '9' -> Right 0x9
    'A' -> Right 0xA
    'B' -> Right 0xB
    'C' -> Right 0xC
    'D' -> Right 0xD
    'E' -> Right 0xE
    'F' -> Right 0xF
    _ -> Left "Hex Digit is not in [0..F]"
unhex _ = Left $ "Word8 Hex Literal did not match the expected format."

prettyHex :: Word8 -> LazyTextBuilder
prettyHex w = "0x" <> LazyTextBuilder.fromText (hex w)
