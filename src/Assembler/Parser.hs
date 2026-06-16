{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Assembler.Parser (program, Decl (..)) where

import Assembler.Instruction (BranchCond (BranchCondFlag, BranchUncond), BranchFlag (..), ConstRef (..), FlagCond (..), Instruction (..), OpSrc (..))
import Control.Applicative (Alternative (empty, many))
import Control.Monad (void)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void (Void)
import Data.Word (Word8)
import Text.Megaparsec (ParseErrorBundle, Parsec, choice, eof, option, runParser, some)
import Text.Megaparsec.Char (alphaNumChar, char, space1)
import Text.Megaparsec.Char.Lexer (binary, decimal, hexadecimal, octal, skipLineComment, space, symbol')

type Parser = Parsec Void Text

data Decl
  = DeclLabel Text
  | DeclOffset Word8
  | DeclInst Instruction
  | DeclBytes ByteString

ignore :: Parser ()
ignore = space space1 (skipLineComment ";") empty

sym :: Text -> Parser ()
sym name = symbol' ignore name *> pure ()

comma :: Parser ()
comma = sym ","

number :: Parser Word
number =
  choice
    [ "0x" *> hexadecimal
    , "0b" *> binary
    , "0o" *> octal
    , decimal
    ]
    <* ignore

number8 :: Parser Word8
number8 = do
  n <- number
  if n > 255
    then fail $ "Numeric argument out of range: " ++ show n
    else pure $ fromIntegral n

parseConst :: Parser ConstRef
parseConst =
  choice
    [ KnownConst <$> number8
    , do
        void $ char '@'
        LabelConst
          <$> ident
          <*> ( option 0 $ do
                  void $ char '+'
                  number8
              )
    ]

-- >>> program "hlt"
-- (Right (fromList [Halt]),fromList [])

program :: Text -> Either (ParseErrorBundle Text Void) (Seq Decl)
program source = runParser file "<input>" source

file :: Parser (Seq Decl)
file = fmap Seq.fromList $ ignore *> many decl <* eof

decl :: Parser Decl
decl =
  choice
    [ DeclInst <$> instruction
    , DeclLabel <$> label
    , DeclOffset <$> offset
    , DeclBytes <$> bytes
    ]
 where
  bytes = do
    sym "$bytes"
    sym "["
    bs <- ByteString.pack <$> some number8
    sym "]"
    pure bs
  offset = do
    sym "$offset"
    off <- number8
    sym ":"
    pure off
  label = do
    void $ char '@'
    name <- ident
    sym ":"
    pure name

ident :: Parser Text
ident = Text.pack <$> some alphaNumChar <* ignore

instruction :: Parser Instruction
instruction =
  choice
    [ sym "mov" *> parseMov
    , sym "mvi" *> parseMvi
    , fmap LoadA $ sym "lda" *> parseConst
    , fmap StoreA $ sym "sta" *> parseConst
    , fmap LxiSpN $ sym "lxi" *> sym "sp" *> comma *> parseConst
    , sym "push" *> parsePush
    , sym "pop" *> parsePop
    , InrA <$ (sym "inr" *> sym "A")
    , InrL <$ (sym "inr" *> sym "L")
    , DcrA <$ (sym "dcr" *> sym "A")
    , DcrL <$ (sym "dcr" *> sym "L")
    , fmap In $ sym "in" *> parseConst
    , fmap Out $ sym "out" *> parseConst
    , fmap Add $ sym "add" *> parseOpSrc
    , fmap Sub $ sym "sub" *> parseOpSrc
    , fmap Cmp $ sym "cmp" *> parseOpSrc
    , fmap And $ sym "and" *> parseOpSrc
    , fmap Or $ sym "or" *> parseOpSrc
    , fmap Xor $ sym "xor" *> parseOpSrc
    , withAddr "jmp" $ Jump BranchUncond
    , withAddr "jz" $ Jump $ BranchCondFlag BranchFlagZero FlagCondSet
    , withAddr "jnz" $ Jump $ BranchCondFlag BranchFlagZero FlagCondUnset
    , withAddr "jc" $ Jump $ BranchCondFlag BranchFlagCarry FlagCondSet
    , withAddr "jnc" $ Jump $ BranchCondFlag BranchFlagCarry FlagCondUnset
    , withAddr "call" $ Call $ BranchUncond
    , withAddr "cz" $ Jump $ BranchCondFlag BranchFlagZero FlagCondSet
    , withAddr "cnz" $ Jump $ BranchCondFlag BranchFlagZero FlagCondUnset
    , withAddr "cc" $ Jump $ BranchCondFlag BranchFlagCarry FlagCondSet
    , withAddr "cnc" $ Jump $ BranchCondFlag BranchFlagCarry FlagCondUnset
    , Ret <$ sym "ret"
    , Halt <$ sym "hlt"
    , Nop <$ sym "nop"
    , EnableI <$ sym "ei"
    , DisableI <$ sym "di"
    ]
 where
  withAddr n f = fmap f $ sym n *> parseConst
  parseOpSrc =
    choice
      [ OpSrcAccu <$ sym "A"
      , OpSrcReg <$ sym "L"
      , OpSrcMem <$ sym "M"
      , OpSrcConst <$> parseConst
      ]
  parsePush =
    choice
      [ sym "A" *> pure PushA
      , sym "L" *> pure PushL
      , sym "FL" *> pure PushFL
      ]
  parsePop =
    choice
      [ sym "A" *> pure PopA
      , sym "L" *> pure PopL
      , sym "FL" *> pure PopFL
      ]
  parseMvi =
    choice
      [ fmap MviAN $ sym "A" *> comma *> parseConst
      , fmap MviLN $ sym "L" *> comma *> parseConst
      ]
  parseMov =
    choice
      [ sym "A"
          *> comma
          *> choice
            [ sym "L" *> pure MovAL
            , sym "M" *> pure MovAM
            ]
      , do
          sym "M"
          comma
          sym "A"
          pure MovMA
      , sym "L"
          *> comma
          *> choice
            [ sym "A" *> pure MovLA
            , sym "M" *> pure MovLM
            ]
      ]
