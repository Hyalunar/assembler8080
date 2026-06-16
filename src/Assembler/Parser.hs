{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Assembler.Parser (program) where

import Assembler.Instruction (BranchCond (BranchCondFlag, BranchUncond), BranchFlag (..), ConstRef (..), FlagCond (..), Instruction (..), OpSrc (..), size)
import Control.Applicative (Alternative (empty, many))
import Control.Monad (void, when)
import Control.Monad.State (MonadTrans (lift), State, gets, modify, runState)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void (Void)
import Data.Word (Word8)
import Text.Megaparsec (ParseErrorBundle, ParsecT, choice, eof, runParserT, some)
import Text.Megaparsec.Char (alphaNumChar, char, space1)
import Text.Megaparsec.Char.Lexer (binary, decimal, hexadecimal, octal, skipLineComment, space, symbol')

type Parser = ParsecT Void Text (State ParserState)

data ParserState
  = ParserState
  { offset :: Word8
  , labels :: Map Text Word8 -- name to offset
  }

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

parseConst :: Parser ConstRef
parseConst =
  choice
    [ do
        n <- number
        if n > 255
          then fail $ "Numeric argument out of range: " ++ show n
          else pure . KnownConst $ fromIntegral n
    , do
        void $ char '@'
        LabelConst <$> ident <* ignore
    ]

-- >>> program "hlt"
-- (Right (fromList [Halt]),fromList [])

program :: Text -> (Either (ParseErrorBundle Text Void) (Seq Instruction), Map Text Word8)
program source =
  let
    initialState =
      ParserState
        { offset = 0
        , labels = mempty
        }
   in
    labels <$> runState (runParserT file "<input>" source) initialState

file :: Parser (Seq Instruction)
file = fmap Seq.fromList $ ignore *> many decl <* eof

decl :: Parser Instruction
decl = do
  inst <- instruction
  lift (modify $ incInstCount inst)
  void $ many $ label
  pure inst
 where
  label = do
    void $ char '@'
    name <- ident
    sym ":"

    -- check for duplicates
    alreadyDefined <- Map.member name <$> gets labels
    when alreadyDefined . fail $ "Label is defined twice: " ++ show name

    -- register new label
    pos <- gets offset
    lift $ modify $ addLabel pos name
  addLabel p n s = s{labels = Map.insert n p s.labels}
  incInstCount inc p = p{offset = size inc + p.offset}

ident :: Parser Text
ident = Text.pack <$> some alphaNumChar

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
