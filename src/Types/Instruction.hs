{-# LANGUAGE DeriveGeneric #-}
module Types.Instruction where

import GHC.Generics (Generic)
import Data.List (elemIndex)
import Hardware.Microcode

data Address = ALiteral
                | AAbsolute
                | AIndirect
                | AIndexed
                | APreIndex
                | APostIndex
                deriving (Show, Eq, Generic)

data ExtendedOp = EAlpha | EBeta | EStackP deriving (Show, Eq, Generic)
data MoveOp = AtB | BtA | SPtA | AtSP | SPtB | BtSP deriving (Show, Eq, Generic)
data Register = RAlpha | RBeta deriving (Show, Eq, Generic)
data JmpAddress = SAbsolute | SIndirect deriving (Show, Eq, Generic)

data TieInstruction = NoOp
                      -- Move group
                    | Move MoveOp
                      -- Extdned register ops
                    | Inc ExtendedOp
                    | Dec ExtendedOp
                      -- Register only ops
                    | Yes Register
                    | No Register
                    | Not Register
                    | Negate Register
                    | Push Register
                    | Pop Register
                    | Shift Register
                      -- Special ops
                    | Halt
                    | SetCarry
                    | ClrCarry
                    | SetInt
                    | ClrInt
                    | Return
                    | Nop
                      -- Relative jumps
                    | BEqual
                    | BNotEqual
                    | BLess
                    | BLessEqual
                    | BGrt
                    | BGrtEqual
                    | BInt
                    | BNotInt
                    | BCarry
                    | BNotCarry
                    | BAlways
                    | BPositive
                    | BNegative
                    | BZero
                    | BNotZero
                      -- Memory value ops
                    | Add Register Address
                    | AddC Register Address
                    | Subtract Register Address
                    | SubtractC Register Address
                    | Compare Register Address
                    | Store Register Address
                    | Load Register Address
                    | Or Register Address
                    | Nor Register Address
                    | And Register Address
                    | NAnd Register Address
                    | Xor Register Address
                    | NXor Register Address
                      -- Long jumps
                    | Jump JmpAddress
                    | JumpSub JmpAddress
                    deriving (Show, Eq, Generic)

-- TODO: Add to enumerable
nextSimple :: (Defaults x, Eq x) => x -> (x, Bool)
nextSimple x = if ix + 1 == length tdefs
                  then (head tdefs, True)
                  else (tdefs !! (ix + 1), False)
    where (Just ix) = elemIndex x defs
          tdefs = defs `asTypeOf` (return x)
instance Enumerable ExtendedOp where
    per = nextSimple
instance Enumerable Address where
    per = nextSimple
instance Enumerable MoveOp where
    per = nextSimple
instance Enumerable JmpAddress where
    per = nextSimple
instance Enumerable Register where
    per = nextSimple
instance Defaults Register where
    defs = [RAlpha, RBeta]
instance Defaults Address where
    defs = [ALiteral, AAbsolute, AIndirect, AIndexed, APreIndex, APostIndex]
instance Defaults JmpAddress where
    defs = [SAbsolute, SIndirect]
instance Defaults MoveOp where
    defs = [AtB, BtA, SPtA, AtSP, SPtB, BtSP]
instance Defaults ExtendedOp where
    defs = [EAlpha, EBeta, EStackP]
