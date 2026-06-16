{-# LANGUAGE OverloadedStrings #-}

module Assembler.Run (run) where

import Assembler.Compile (csv, display)
import Assembler.Parser (program)
import Control.Monad (forM_)
import qualified Data.Text.IO as IO
import Options.Applicative (ParserInfo, command, execParser, header, hsubparser, info)
import Text.Megaparsec (errorBundlePretty)

data Command
  = Annotate
  | Csv

pInfo :: ParserInfo Command
pInfo =
  info
    ( hsubparser $
        command "annotate" (info (pure Annotate) $ header "Annotate the instructions with hex assembly")
          <> command "csv" (info (pure Csv) $ header "Dump a csv viable for loading with tu-ads")
    )
    (header "Select an operation mode")

run :: IO ()
run = do
  opts <- execParser pInfo
  source <- IO.getContents
  case program source of
    Left failure -> putStr $ errorBundlePretty failure
    Right decls -> case opts of
      Annotate -> forM_ (display decls) $ \(hex, inst) -> do
        IO.putStr $ hex <> ": "
        print inst
      Csv -> do
        IO.putStr $ csv decls
