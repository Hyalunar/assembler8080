{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Assembler.Compile (display)
import Assembler.Parser (program)
import Control.Monad (forM_)
import qualified Data.Text.IO as IO
import Text.Megaparsec

main :: IO ()
main = do
  source <- IO.getContents
  case program source of
    (Left failure, _) -> putStr $ errorBundlePretty failure
    (Right program, labels) -> do
      forM_ (display program labels) $ \(hex, inst) -> do
        IO.putStr $ hex <> ": "
        print inst
