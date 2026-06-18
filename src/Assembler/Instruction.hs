{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Assembler.Instruction (
  Instruction (..),
  OpSrc (..),
  BranchCond (..),
  BranchFlag (..),
  FlagCond (..),
  ConstRef (..),
  assemble,
  size,
  disassemble,
  unparse,
) where

import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as Builder
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Lazy.Builder (LazyTextBuilder)
import qualified Data.Text.Lazy.Builder as Text.Lazy.Builder
import Data.Word (Word8)
import Data.Word8 (prettyHex)

data Instruction
  = -- \* transfer instructions

    -- | move reg to accu
    MovAL
  | -- | load indirect from memory
    MovAM
  | -- | store indirect to memory
    MovMA
  | -- | store constant in accu
    MviAN ConstRef
  | -- | load accu from address
    LoadA ConstRef
  | -- | store accu to address
    StoreA ConstRef
  | -- | move accu to register
    MovLA
  | -- | load register from register address
    MovLM
  | -- | move constant to register
    MviLN ConstRef
  | -- | move constant to stack pointer
    LxiSpN ConstRef
  | -- | decrement sp, store accu
    PushA
  | -- | decrement sp, store register
    PushL
  | -- | decrement sp, store flags
    PushFL
  | -- | load accu, increment sp
    PopA
  | -- | load reg, increment sp
    PopL
  | -- | load flags, increment sp
    PopFL
  | -- | move accu to port address
    In ConstRef
  | -- | move port address to accu
    Out ConstRef
  | -- \* arithmetic instructions

    -- | increment accu
    InrA
  | -- | increment register
    InrL
  | -- | decrement accu
    DcrA
  | -- | decrement register
    DcrL
  | -- | Add to accu
    Add OpSrc
  | -- | subtract from accu
    Sub OpSrc
  | -- | compare accu with
    Cmp OpSrc
  | -- \* Logic instructions

    -- | and accu with
    And OpSrc
  | -- | or accu with
    Or OpSrc
  | -- | xor accu with
    Xor OpSrc
  | -- | jump with condition
    Jump BranchCond ConstRef
  | -- | call with condition
    Call BranchCond ConstRef
  | -- | return from call
    Ret
  | -- | end the program
    Halt
  | -- | do nothing
    Nop
  | -- | Enable interrupt
    EnableI
  | -- | Disable interrupt
    DisableI
  deriving stock (Show)

data ConstRef
  = KnownConst Word8
  | LabelConst Text Word8
  deriving stock (Show)

data BranchCond
  = -- | always jump
    BranchUncond
  | -- | jump depending on flag
    BranchCondFlag BranchFlag FlagCond
  deriving stock (Show)

data BranchFlag
  = BranchFlagZero
  | BranchFlagCarry
  deriving stock (Show)

data FlagCond
  = -- | flag must be set
    FlagCondSet
  | -- | flag may not be set
    FlagCondUnset
  deriving stock (Show)

data OpSrc
  = -- | load from accumulator
    OpSrcAccu
  | -- | load from register
    OpSrcReg
  | -- | load from memory
    OpSrcMem
  | -- | use constant value
    OpSrcConst ConstRef
  deriving stock (Show)

disassemble :: [Word8] -> Maybe (Instruction, [Word8])
disassemble = \case
  [] -> Nothing
  0x7D : rest -> Just (MovAL, rest)
  0x7E : rest -> Just (MovAM, rest)
  0x77 : rest -> Just (MovMA, rest)
  0x3E : n : rest -> Just (MviAN $ KnownConst n, rest)
  0x3A : a : rest -> Just (LoadA $ KnownConst a, rest)
  0x32 : a : rest -> Just (StoreA $ KnownConst a, rest)
  0x6F : rest -> Just (MovLA, rest)
  0x6E : rest -> Just (MovLM, rest)
  0x2E : n : rest -> Just (MviLN $ KnownConst n, rest)
  0x31 : n : rest -> Just (LxiSpN $ KnownConst n, rest)
  0xF5 : rest -> Just (PushA, rest)
  0xE5 : rest -> Just (PushL, rest)
  0xED : rest -> Just (PushFL, rest)
  0xF1 : rest -> Just (PopA, rest)
  0xE1 : rest -> Just (PopL, rest)
  0xFD : rest -> Just (PopFL, rest)
  0xDB : a : rest -> Just (In $ KnownConst a, rest)
  0xD3 : a : rest -> Just (Out $ KnownConst a, rest)
  0x3C : rest -> Just (InrA, rest)
  0x2C : rest -> Just (InrL, rest)
  0x3D : rest -> Just (DcrA, rest)
  0x2D : rest -> Just (DcrL, rest)
  0x85 : rest -> Just (Add OpSrcReg, rest)
  0x86 : rest -> Just (Add OpSrcMem, rest)
  0x87 : rest -> Just (Add OpSrcAccu, rest)
  0xC6 : n : rest -> Just (Add $ OpSrcConst $ KnownConst n, rest)
  0x95 : rest -> Just (Sub OpSrcReg, rest)
  0x96 : rest -> Just (Sub OpSrcMem, rest)
  0x97 : rest -> Just (Sub OpSrcAccu, rest)
  0xD6 : n : rest -> Just (Sub $ OpSrcConst $ KnownConst n, rest)
  0xBD : rest -> Just (Cmp OpSrcReg, rest)
  0xBE : rest -> Just (Cmp OpSrcMem, rest)
  0xBF : rest -> Just (Cmp OpSrcAccu, rest)
  0xFE : n : rest -> Just (Cmp $ OpSrcConst $ KnownConst n, rest)
  0xA5 : rest -> Just (And OpSrcReg, rest)
  0xA6 : rest -> Just (And OpSrcMem, rest)
  0xA7 : rest -> Just (And OpSrcAccu, rest)
  0xE6 : n : rest -> Just (And $ OpSrcConst $ KnownConst n, rest)
  0xB5 : rest -> Just (Or OpSrcReg, rest)
  0xB6 : rest -> Just (Or OpSrcMem, rest)
  0xB7 : rest -> Just (Or OpSrcAccu, rest)
  0xF6 : n : rest -> Just (Or $ OpSrcConst $ KnownConst n, rest)
  0xAD : rest -> Just (Xor OpSrcReg, rest)
  0xAE : rest -> Just (Xor OpSrcMem, rest)
  0xAF : rest -> Just (Xor OpSrcAccu, rest)
  0xEE : n : rest -> Just (Xor $ OpSrcConst $ KnownConst n, rest)
  0xC3 : a : rest -> Just (Jump BranchUncond $ KnownConst a, rest)
  0xCA : a : rest -> Just (Jump (BranchCondFlag BranchFlagZero FlagCondSet) $ KnownConst a, rest)
  0xC2 : a : rest -> Just (Jump (BranchCondFlag BranchFlagZero FlagCondUnset) $ KnownConst a, rest)
  0xDA : a : rest -> Just (Jump (BranchCondFlag BranchFlagCarry FlagCondSet) $ KnownConst a, rest)
  0xD2 : a : rest -> Just (Jump (BranchCondFlag BranchFlagCarry FlagCondUnset) $ KnownConst a, rest)
  0xCD : a : rest -> Just (Call BranchUncond $ KnownConst a, rest)
  0xCC : a : rest -> Just (Call (BranchCondFlag BranchFlagZero FlagCondSet) $ KnownConst a, rest)
  0xC4 : a : rest -> Just (Call (BranchCondFlag BranchFlagZero FlagCondUnset) $ KnownConst a, rest)
  0xDC : a : rest -> Just (Call (BranchCondFlag BranchFlagCarry FlagCondSet) $ KnownConst a, rest)
  0xD4 : a : rest -> Just (Call (BranchCondFlag BranchFlagCarry FlagCondUnset) $ KnownConst a, rest)
  0xC9 : rest -> Just (Ret, rest)
  0x76 : rest -> Just (Halt, rest)
  0x00 : rest -> Just (Nop, rest)
  0xFB : rest -> Just (EnableI, rest)
  0xF3 : rest -> Just (DisableI, rest)
  _ -> Nothing

assemble :: (Text -> Maybe Word8) -> Instruction -> Either Text Builder
assemble lblPos = \case
  MovAL -> Right $ Builder.word8 0x7D
  MovAM -> Right $ Builder.word8 0x7E
  MovMA -> Right $ Builder.word8 0x77
  MviAN n -> foldMapM Builder.word8 $ [Right 0x3E, get n]
  LoadA a -> foldMapM Builder.word8 $ [Right 0x3A, get a]
  StoreA a -> foldMapM Builder.word8 $ [Right 0x32, get a]
  MovLA -> Right $ Builder.word8 0x6F
  MovLM -> Right $ Builder.word8 0x6E
  MviLN n -> foldMapM Builder.word8 $ [Right 0x2E, get n]
  LxiSpN n -> foldMapM Builder.word8 $ [Right 0x31, get n]
  PushA -> Right $ Builder.word8 0xF5
  PushL -> Right $ Builder.word8 0xE5
  PushFL -> Right $ Builder.word8 0xED
  PopA -> Right $ Builder.word8 0xF1
  PopL -> Right $ Builder.word8 0xE1
  PopFL -> Right $ Builder.word8 0xFD
  In a -> foldMapM Builder.word8 $ [Right 0xDB, get a]
  Out a -> foldMapM Builder.word8 $ [Right 0xD3, get a]
  InrA -> Right $ Builder.word8 0x3C
  InrL -> Right $ Builder.word8 0x2C
  DcrA -> Right $ Builder.word8 0x3D
  DcrL -> Right $ Builder.word8 0x2D
  Add OpSrcReg -> Right $ Builder.word8 0x85
  Add OpSrcMem -> Right $ Builder.word8 0x86
  Add OpSrcAccu -> Right $ Builder.word8 0x87
  Add (OpSrcConst n) -> foldMapM Builder.word8 $ [Right 0xC6, get n]
  Sub OpSrcReg -> Right $ Builder.word8 0x95
  Sub OpSrcMem -> Right $ Builder.word8 0x96
  Sub OpSrcAccu -> Right $ Builder.word8 0x97
  Sub (OpSrcConst n) -> foldMapM Builder.word8 $ [Right 0xD6, get n]
  Cmp OpSrcReg -> Right $ Builder.word8 0xBD
  Cmp OpSrcMem -> Right $ Builder.word8 0xBE
  Cmp OpSrcAccu -> Right $ Builder.word8 0xBF
  Cmp (OpSrcConst n) -> foldMapM Builder.word8 $ [Right 0xFE, get n]
  And OpSrcReg -> Right $ Builder.word8 0xA5
  And OpSrcMem -> Right $ Builder.word8 0xA6
  And OpSrcAccu -> Right $ Builder.word8 0xA7
  And (OpSrcConst n) -> foldMapM Builder.word8 $ [Right 0xE6, get n]
  Or OpSrcReg -> Right $ Builder.word8 0xB5
  Or OpSrcMem -> Right $ Builder.word8 0xB6
  Or OpSrcAccu -> Right $ Builder.word8 0xB7
  Or (OpSrcConst n) -> foldMapM Builder.word8 $ [Right 0xF6, get n]
  Xor OpSrcReg -> Right $ Builder.word8 0xAD
  Xor OpSrcMem -> Right $ Builder.word8 0xAE
  Xor OpSrcAccu -> Right $ Builder.word8 0xAF
  Xor (OpSrcConst n) -> foldMapM Builder.word8 $ [Right 0xEE, get n]
  Jump BranchUncond a -> foldMapM Builder.word8 $ [Right 0xC3, get a]
  Jump (BranchCondFlag BranchFlagZero FlagCondSet) a -> foldMapM Builder.word8 $ [Right 0xCA, get a]
  Jump (BranchCondFlag BranchFlagZero FlagCondUnset) a -> foldMapM Builder.word8 $ [Right 0xC2, get a]
  Jump (BranchCondFlag BranchFlagCarry FlagCondSet) a -> foldMapM Builder.word8 $ [Right 0xDA, get a]
  Jump (BranchCondFlag BranchFlagCarry FlagCondUnset) a -> foldMapM Builder.word8 $ [Right 0xD2, get a]
  Call BranchUncond a -> foldMapM Builder.word8 $ [Right 0xCD, get a]
  Call (BranchCondFlag BranchFlagZero FlagCondSet) a -> foldMapM Builder.word8 $ [Right 0xCC, get a]
  Call (BranchCondFlag BranchFlagZero FlagCondUnset) a -> foldMapM Builder.word8 $ [Right 0xC4, get a]
  Call (BranchCondFlag BranchFlagCarry FlagCondSet) a -> foldMapM Builder.word8 $ [Right 0xDC, get a]
  Call (BranchCondFlag BranchFlagCarry FlagCondUnset) a -> foldMapM Builder.word8 $ [Right 0xD4, get a]
  Ret -> Right $ Builder.word8 0xC9
  Halt -> Right $ Builder.word8 0x76
  Nop -> Right $ Builder.word8 0x00
  EnableI -> Right $ Builder.word8 0xFB
  DisableI -> Right $ Builder.word8 0xF3
 where
  get = \case
    KnownConst w -> Right w
    LabelConst n off -> case lblPos n of
      Nothing -> Left $ "Label " <> n <> " is referenced but never defined"
      Just pos -> Right $ pos + off

foldMapM :: (Monoid m, Traversable t, Monad f) => (a -> m) -> t (f a) -> f m
foldMapM f = fmap (foldMap f) . sequence

size :: Instruction -> Word8
size = \case
  MovAL -> 1
  MovAM -> 1
  MovMA -> 1
  MviAN _ -> 2
  LoadA _ -> 2
  StoreA _ -> 2
  MovLA -> 1
  MovLM -> 1
  MviLN _ -> 2
  LxiSpN _ -> 2
  PushA -> 1
  PushL -> 1
  PushFL -> 1
  PopA -> 1
  PopL -> 1
  PopFL -> 1
  In _ -> 2
  Out _ -> 2
  InrA -> 1
  InrL -> 1
  DcrA -> 1
  DcrL -> 1
  Add OpSrcReg -> 1
  Add OpSrcMem -> 1
  Add OpSrcAccu -> 1
  Add (OpSrcConst _) -> 2
  Sub OpSrcReg -> 1
  Sub OpSrcMem -> 1
  Sub OpSrcAccu -> 1
  Sub (OpSrcConst _) -> 2
  Cmp OpSrcReg -> 1
  Cmp OpSrcMem -> 1
  Cmp OpSrcAccu -> 1
  Cmp (OpSrcConst _) -> 2
  And OpSrcReg -> 1
  And OpSrcMem -> 1
  And OpSrcAccu -> 1
  And (OpSrcConst _) -> 2
  Or OpSrcReg -> 1
  Or OpSrcMem -> 1
  Or OpSrcAccu -> 1
  Or (OpSrcConst _) -> 2
  Xor OpSrcReg -> 1
  Xor OpSrcMem -> 1
  Xor OpSrcAccu -> 1
  Xor (OpSrcConst _) -> 2
  Jump BranchUncond _ -> 2
  Jump (BranchCondFlag BranchFlagZero FlagCondSet) _ -> 2
  Jump (BranchCondFlag BranchFlagZero FlagCondUnset) _ -> 2
  Jump (BranchCondFlag BranchFlagCarry FlagCondSet) _ -> 2
  Jump (BranchCondFlag BranchFlagCarry FlagCondUnset) _ -> 2
  Call BranchUncond _ -> 2
  Call (BranchCondFlag BranchFlagZero FlagCondSet) _ -> 2
  Call (BranchCondFlag BranchFlagZero FlagCondUnset) _ -> 2
  Call (BranchCondFlag BranchFlagCarry FlagCondSet) _ -> 2
  Call (BranchCondFlag BranchFlagCarry FlagCondUnset) _ -> 2
  Ret -> 1
  Halt -> 1
  Nop -> 1
  EnableI -> 1
  DisableI -> 1

unparse :: Instruction -> LazyTextBuilder
unparse = \case
  MovAL -> "mov a, l"
  MovAM -> "mov a, m"
  MovMA -> "mov m, a"
  MviAN c -> "mvi a, " <> unparseConstRef c
  LoadA c -> "lda " <> unparseConstRef c
  StoreA c -> "sta " <> unparseConstRef c
  MovLA -> "mov l, a"
  MovLM -> "mov l, m"
  MviLN c -> "mvi l, " <> unparseConstRef c
  LxiSpN c -> "lxi sp, " <> unparseConstRef c
  PushA -> "push a"
  PushL -> "push l"
  PushFL -> "push fl"
  PopA -> "pop a"
  PopL -> "pop l"
  PopFL -> "pop fl"
  In c -> "in " <> unparseConstRef c
  Out c -> "out " <> unparseConstRef c
  InrA -> "inr a"
  InrL -> "inr l"
  DcrA -> "dcr a"
  DcrL -> "dcr l"
  Add OpSrcReg -> "add l"
  Add OpSrcMem -> "add m"
  Add OpSrcAccu -> "add a"
  Add (OpSrcConst c) -> "add " <> unparseConstRef c
  Sub OpSrcReg -> "sub l"
  Sub OpSrcMem -> "sub m"
  Sub OpSrcAccu -> "sub a"
  Sub (OpSrcConst c) -> "sub " <> unparseConstRef c
  Cmp OpSrcReg -> "cmp l"
  Cmp OpSrcMem -> "cmp m"
  Cmp OpSrcAccu -> "cmp a"
  Cmp (OpSrcConst c) -> "cmp " <> unparseConstRef c
  And OpSrcReg -> "and l"
  And OpSrcMem -> "and m"
  And OpSrcAccu -> "and a"
  And (OpSrcConst c) -> "and " <> unparseConstRef c
  Or OpSrcReg -> "or l"
  Or OpSrcMem -> "or m"
  Or OpSrcAccu -> "or a"
  Or (OpSrcConst c) -> "or " <> unparseConstRef c
  Xor OpSrcReg -> "xor l"
  Xor OpSrcMem -> "xor m"
  Xor OpSrcAccu -> "xor a"
  Xor (OpSrcConst c) -> "xor " <> unparseConstRef c
  Jump BranchUncond c -> "jmp " <> unparseConstRef c
  Jump (BranchCondFlag BranchFlagZero FlagCondSet) c -> "jz " <> unparseConstRef c
  Jump (BranchCondFlag BranchFlagZero FlagCondUnset) c -> "jnz " <> unparseConstRef c
  Jump (BranchCondFlag BranchFlagCarry FlagCondSet) c -> "jc " <> unparseConstRef c
  Jump (BranchCondFlag BranchFlagCarry FlagCondUnset) c -> "jnc " <> unparseConstRef c
  Call BranchUncond c -> "call " <> unparseConstRef c
  Call (BranchCondFlag BranchFlagZero FlagCondSet) c -> "cz " <> unparseConstRef c
  Call (BranchCondFlag BranchFlagZero FlagCondUnset) c -> "cnz " <> unparseConstRef c
  Call (BranchCondFlag BranchFlagCarry FlagCondSet) c -> "cc " <> unparseConstRef c
  Call (BranchCondFlag BranchFlagCarry FlagCondUnset) c -> "cnc " <> unparseConstRef c
  Ret -> "ret"
  Halt -> "hlt"
  Nop -> "nop"
  EnableI -> "ei"
  DisableI -> "di"

unparseConstRef :: ConstRef -> LazyTextBuilder
unparseConstRef = \case
  KnownConst w -> prettyHex w
  LabelConst name off ->
    mconcat
      [ "@"
      , Text.Lazy.Builder.fromText name
      , "+"
      , Text.Lazy.Builder.fromText $ Text.show off
      ]
