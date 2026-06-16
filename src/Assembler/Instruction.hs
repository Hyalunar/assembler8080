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
) where

import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as Builder
import Data.Text (Text)
import Data.Word (Word8)

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

assemble :: (Text -> Word8) -> Instruction -> Builder
assemble lblPos = \case
  MovAL -> Builder.word8 0x7D
  MovAM -> Builder.word8 0x7E
  MovMA -> Builder.word8 0x77
  MviAN n -> Builder.word8 0x3E <> Builder.word8 (get n)
  LoadA a -> Builder.word8 0x3A <> Builder.word8 (get a)
  StoreA a -> Builder.word8 0x32 <> Builder.word8 (get a)
  MovLA -> Builder.word8 0x6F
  MovLM -> Builder.word8 0x6E
  MviLN n -> Builder.word8 0x2E <> Builder.word8 (get n)
  LxiSpN n -> Builder.word8 0x31 <> Builder.word8 (get n)
  PushA -> Builder.word8 0xF5
  PushL -> Builder.word8 0xE5
  PushFL -> Builder.word8 0xED
  PopA -> Builder.word8 0xF1
  PopL -> Builder.word8 0xE1
  PopFL -> Builder.word8 0xFD
  In a -> Builder.word8 0xDB <> Builder.word8 (get a)
  Out a -> Builder.word8 0xD3 <> Builder.word8 (get a)
  InrA -> Builder.word8 0x3C
  InrL -> Builder.word8 0x2C
  DcrA -> Builder.word8 0x3D
  DcrL -> Builder.word8 0x2D
  Add OpSrcReg -> Builder.word8 0x85
  Add OpSrcMem -> Builder.word8 0x86
  Add OpSrcAccu -> Builder.word8 0x87
  Add (OpSrcConst n) -> Builder.word8 0xC6 <> Builder.word8 (get n)
  Sub OpSrcReg -> Builder.word8 0x95
  Sub OpSrcMem -> Builder.word8 0x96
  Sub OpSrcAccu -> Builder.word8 0x97
  Sub (OpSrcConst n) -> Builder.word8 0xD6 <> Builder.word8 (get n)
  Cmp OpSrcReg -> Builder.word8 0xBD
  Cmp OpSrcMem -> Builder.word8 0xBE
  Cmp OpSrcAccu -> Builder.word8 0xBF
  Cmp (OpSrcConst n) -> Builder.word8 0xFE <> Builder.word8 (get n)
  And OpSrcReg -> Builder.word8 0xA5
  And OpSrcMem -> Builder.word8 0xA6
  And OpSrcAccu -> Builder.word8 0xA7
  And (OpSrcConst n) -> Builder.word8 0xE6 <> Builder.word8 (get n)
  Or OpSrcReg -> Builder.word8 0xB5
  Or OpSrcMem -> Builder.word8 0xB6
  Or OpSrcAccu -> Builder.word8 0xB7
  Or (OpSrcConst n) -> Builder.word8 0xF6 <> Builder.word8 (get n)
  Xor OpSrcReg -> Builder.word8 0xAD
  Xor OpSrcMem -> Builder.word8 0xAE
  Xor OpSrcAccu -> Builder.word8 0xAF
  Xor (OpSrcConst n) -> Builder.word8 0xEE <> Builder.word8 (get n)
  Jump BranchUncond a -> Builder.word8 0xC3 <> Builder.word8 (get a)
  Jump (BranchCondFlag BranchFlagZero FlagCondSet) a -> Builder.word8 0xCA <> Builder.word8 (get a)
  Jump (BranchCondFlag BranchFlagZero FlagCondUnset) a -> Builder.word8 0xC2 <> Builder.word8 (get a)
  Jump (BranchCondFlag BranchFlagCarry FlagCondSet) a -> Builder.word8 0xDA <> Builder.word8 (get a)
  Jump (BranchCondFlag BranchFlagCarry FlagCondUnset) a -> Builder.word8 0xD2 <> Builder.word8 (get a)
  Call BranchUncond a -> Builder.word8 0xCD <> Builder.word8 (get a)
  Call (BranchCondFlag BranchFlagZero FlagCondSet) a -> Builder.word8 0xCC <> Builder.word8 (get a)
  Call (BranchCondFlag BranchFlagZero FlagCondUnset) a -> Builder.word8 0xC4 <> Builder.word8 (get a)
  Call (BranchCondFlag BranchFlagCarry FlagCondSet) a -> Builder.word8 0xDC <> Builder.word8 (get a)
  Call (BranchCondFlag BranchFlagCarry FlagCondUnset) a -> Builder.word8 0xD4 <> Builder.word8 (get a)
  Ret -> Builder.word8 0xC9
  Halt -> Builder.word8 0x76
  Nop -> Builder.word8 0x00
  EnableI -> Builder.word8 0xFB
  DisableI -> Builder.word8 0xF3
 where
  get = \case
    KnownConst w -> w
    LabelConst n off -> off + lblPos n

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
