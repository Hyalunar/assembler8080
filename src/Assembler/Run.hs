{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Assembler.Run (run) where

import Assembler.Compile (collectLabels, csv, uncsv)
import qualified Assembler.Instruction as Instruction
import Assembler.Parser (Decl (..), prettyDecl, program)
import Control.Arrow ((>>>))
import qualified Data.ByteString.Builder as ByteString.Builder
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Sequence (Seq)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as IO
import Data.Text.Lazy.Builder (LazyTextBuilder)
import qualified Data.Text.Lazy.Builder as LazyText.Builder
import qualified Data.Text.Lazy.IO as Lazy.IO
import qualified Data.Word8 as Word8
import Options.Applicative (ParserInfo, command, customExecParser, header, helper, hsubparser, info, prefs, progDesc, showHelpOnEmpty, showHelpOnError)
import System.Exit (exitFailure)
import Text.Megaparsec (errorBundlePretty)

data Command
  = Annotate
  | Csv
  | UnCsv

pInfo :: ParserInfo Command
pInfo =
  info
    ( helper
        <*> ( hsubparser $
                mconcat
                  [ command "annotate" (info (pure Annotate) $ progDesc "Annotate the instructions with hex assembly")
                  , command "csv" (info (pure Csv) $ progDesc "Dump a csv viable for loading with tu-ads")
                  , command "uncsv" (info (pure UnCsv) $ progDesc "Disassemble from a csv memory dump")
                  ]
            )
    )
    (header "Select an operation mode")

run :: IO ()
run = do
  opts <- customExecParser (prefs $ showHelpOnEmpty <> showHelpOnError) pInfo
  source <- IO.getContents
  case opts of
    Csv -> do
      decls <- programOrDie source
      IO.putStrLn $ csv decls
    UnCsv -> do
      decls <- eitherDieOr $ uncsv source
      Lazy.IO.putStrLn $ LazyText.Builder.toLazyText $ prettyProgram decls
    Annotate -> do
      decls <- programOrDie source
      Lazy.IO.putStrLn $ LazyText.Builder.toLazyText $ annotateProgram decls

annotateProgram :: Seq Decl -> LazyText.Builder.Builder
annotateProgram decls = mconcat . List.intersperse "\n" . fmap annotateDecl . Foldable.toList $ decls
 where
  labels = collectLabels decls
  annotateDecl :: Decl -> LazyText.Builder.Builder
  annotateDecl = \case
    DeclInst inst ->
      let
        assemblyBytes =
          const inst
            >>> Instruction.assemble (labels Map.!)
            >>> ByteString.Builder.toLazyByteString
            >>> ByteString.Lazy.unpack
            >>> fmap (LazyText.Builder.fromText . Word8.hex)
            >>> mconcat
            $ ()
       in
        mconcat
          [ assemblyBytes
          , ":"
          , prettyDecl $ DeclInst inst
          ]
    decl -> prettyDecl decl

prettyProgram :: [Decl] -> LazyTextBuilder
prettyProgram = mconcat . List.intersperse "\n" . fmap prettyDecl

eitherDieOr :: Either Text a -> IO a
eitherDieOr (Left e) = IO.putStrLn e *> exitFailure
eitherDieOr (Right a) = pure a

programOrDie :: Text -> IO (Seq Decl)
programOrDie source = case program source of
  Left e -> do
    IO.putStr . Text.pack $ errorBundlePretty e
    exitFailure
  Right decls -> pure decls
