{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
module Types.Ucode where

import Hardware.Microcode
import Types.Instruction
import GHC.Generics (Generic)

type Bit3 = (Bit, Bit, Bit)
type Bit5 = (Bit, Bit, Bit, Bit, Bit)

data TieFlags = TieFlags { flagInt :: Bit
                         , flagEqual :: Bit
                         , flagCarry :: Bit
                         , flagZero :: Bit
                         , flagSign :: Bit
                         } deriving (Show, Generic)

data TieSignal = TieSignal { -- Alu inputs (5)
                             carryIn_ :: Bit
                           , aluFunc :: Bit5 -- Mode first
                             -- Ucode controller (4)
                           , ucodeCtrl :: Bit3
                           , incPc :: Bit
                             -- Register outputs (3)
                           , aluIn_ :: Bit3
                             -- Register enables (5)
                           , weRam_ :: Bit
                           , wePc_ :: Bit
                           , weStd :: Bit3
                             -- Bus controll (5)
                           , aBusFetch :: Bit
                           , bBusZero :: Bit
                           , vBusStd :: Bit3
                           } deriving (Show)

data UcodeFunc = URstState
               | UHalt
               | UClrInt
               | USetInt
               | UWeFlags
               | UWeInstReg
               | UNone

data AluFunc = FAdd --FIXME
             | FLess
             | FJustA
             | FJustB
             | FZero
             | FOne
             | FInc
             | FAdd
             deriving (Show)

data WriteDest = WAlpha
               | WBeta
               | WGamma
               | WFetchLow
               | WFetch
               | WPcLow
               | WLedBank
               | WNullOut
               deriving (Show)

data ValueSrc = SwitchBank
              | RamVal
              | AluVal
              | PcValLow
              | PcVaLHigh
              deriving (Show)

instance Enumerable TieFlags

instance Default TieFlags where
    def = TieFlags Low Low Low Low Low
instance Default TieSignal where
    def = TieSignal { -- Alu inputs (6)
                      carryIn_ = High
                    , aluFunc = (Low, Low, Low, Low, Low) --FIXME
                      -- Ucode controller (4)
                    , ucodeCtrl = ucodeBits UNone
                    , incPc = Low
                      -- Register outputs (3)
                    , aluIn_ = registerBits EAlpha
                      -- Register enables (5)
                    , weRam_ = High
                    , wePc_ = High
                    , weStd = writeBits WNullOut
                      -- Bus controll (5)
                    , aBusFetch = Low
                    , bBusZero = Low
                    , vBusStd = valueBits AluVal
                    }

instance Flags TieFlags where
    flagBits TieFlags{..} = bit "flagInt" flagInt ++
                            bit "flagEqual" flagEqual ++
                            bit "flagCarry" flagCarry ++
                            bit "flagZero" flagZero ++
                            bit "flagSign" flagSign

instance Signal TieSignal where
    signalBits TieSignal{..} = -- Rom 1
                               bit3 "ucodeCtrl" ucodeCtrl ++
                               bit "wePc_" wePc_ ++
                               bit "weRam_" weRam_ ++
                               bit3 "vBusStd" vBusStd ++
                               -- Rom 2
                               bit5 "aluFunc" aluFunc ++
                               bit "carryIn_" carryIn_ ++
                               bit "bBusZero" bBusZero ++
                               bit "aBusFetch" aBusFetch ++
                               -- Rom 3
                               bit0 ++
                               bit3 "aluIn_" aluIn_ ++
                               bit "incPc" incPc ++
                               bit3 "weStd" weStd

bit3 :: String -> Bit3 -> Bits
bit3 doc (a, b, c) = makeBits doc [a, b, c]

bit5 :: String -> Bit5 -> Bits
bit5 doc (a, b, c, d, e) = makeBits doc [a, b, c, d, e]

writeBits :: WriteDest -> Bit3
writeBits WNullOut   = (Low,  Low,  Low)
writeBits WAlpha     = (High, Low,  Low)
writeBits WBeta      = (Low,  High, Low)
writeBits WGamma     = (High, High, Low)
writeBits WFetch     = (Low,  Low,  High)
writeBits WFetchLow  = (High, Low,  High)
writeBits WPcLow     = (Low,  High, High)
writeBits WLedBank   = (High, High, High)

valueBits :: ValueSrc -> Bit3
valueBits AluVal     = (Low,  Low,  Low)
valueBits RamVal     = (High, Low,  Low)
valueBits SwitchBank = (Low,  High, Low)
valueBits PcValLow   = (High, High, Low)
valueBits PcVaLHigh  = (Low,  Low,  High)

aluBits :: AluFunc -> Bit5
aluBits = undefined

registerBits :: ExtendedOp -> Bit3
registerBits EAlpha  = (Low,  High, High)
registerBits EBeta   = (High, Low,  High)
registerBits EStackP = (High, High, Low)

ucodeBits :: UcodeFunc -> Bit3
ucodeBits UNone       = (Low,  Low,  Low)
ucodeBits URstState   = (High, Low,  Low)
ucodeBits UHalt       = (Low,  High, Low)
ucodeBits UClrInt     = (High, High, Low)
ucodeBits USetInt     = (Low,  Low,  High)
ucodeBits UWeFlags    = (High, Low,  High)
ucodeBits UWeInstReg  = (Low,  High, High)
