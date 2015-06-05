{-# OPTIONS_GHC -fno-warn-orphans #-}
-- This suddenly went really wrong
module Assembly where

import Types.Instruction
import Hardware.Microcode hiding (Bits)

import Data.Word
import Data.Bits
import Numeric (showHex)
import Data.List (nub)

instance Instruction TieInstruction where
    instructions = allEnum
    instructonBits = makeBits "instruction" . intToBit . fromIntegral . encode

instance Default TieInstruction where
    def = Move AtB

instance Enumerable TieInstruction where
    per = nextSimple

instance Defaults TieInstruction where
    defs = nub $ map decode [minBound..maxBound]

(.<<.) :: (Bits a) => a -> Int -> a
(.<<.) = shiftL
(.>>.) :: (Bits a) => a -> Int -> a
(.>>.) = shiftR

hex :: Word8 -> String
hex = flip showHex ""

-- I feel dirty doing this: But it works really weel if every instruction is unqiue
-- Try to fill it in with "mapM_ (\x -> printf "encode (%s) = %s\n" (show x) (show $ encode x)) allEnum"
-- But hard-cording doesn't speed it up
encode :: TieInstruction -> Word8
encode i = head $ filter ((i ==) . decode) [minBound..maxBound]

decode :: Word8 -> TieInstruction
decode w
    | 0 == lead = case post
                  of 0 -> Move AtB
                     1 -> Move BtA
                     2 -> Move SPtA
                     3 -> Move AtSP
                     4 -> Move SPtB
                     5 -> Move BtSP
                     _ -> NoOp
    | 1 == lead = case noreg
                  of 0 -> No reg
                     1 -> Yes reg
                     2 -> Not reg
                     3 -> Negate reg
                     4 -> Push reg
                     5 -> Pop reg
                     6 -> Shift reg
                     _ -> NoOp
    | 2 == lead && w .&. 3 == 3 = NoOp -- Inc/Dec with '4' ext-reg-op
    | 2 == lead = case (w .>>. 2) .&. 3
                  of 0 -> Inc ereg
                     1 -> Dec ereg
                     _ -> NoOp
    | 3 == lead = case post
                  of 0 -> Halt
                     1 -> SetCarry
                     2 -> ClrCarry
                     3 -> Nop
                     4 -> SetInt
                     5 -> ClrInt
                     6 -> Return
                     _ -> NoOp
    | 4 == lead = case post
                  of 0 -> BEqual
                     1 -> BNotEqual
                     2 -> BLess
                     3 -> BLessEqual
                     4 -> BGrt
                     5 -> BGrtEqual
                     6 -> BInt
                     7 -> BNotInt
                     8 -> BCarry
                     9 -> BNotCarry
                     10 -> BAlways
                     11 -> BPositive
                     12 -> BNegative
                     13 -> BZero
                     14 -> BNotZero
                     _ -> NoOp
    | 5 == lead = case post
                  of 0 -> Jump SAbsolute
                     1 -> Jump SIndirect
                     2 -> JumpSub SAbsolute
                     3 -> JumpSub SIndirect
                     _ ->  NoOp
    | otherwise = case post
                  of 0 -> Add areg addr
                     1 -> AddC areg addr
                     2 -> Subtract areg addr
                     3 -> SubtractC areg addr
                     4 -> Compare areg addr
                     5 -> Store areg addr
                     6 -> Load areg addr
                     7 -> Or areg addr
                     8 -> Nor areg addr
                     9 -> And areg addr
                     10 -> NAnd areg addr
                     11 -> Xor areg addr
                     12 -> NXor areg addr
                     _ -> NoOp
    where lead = w .>>. 4 -- Leading nibble
          slead = lead .>>. 1 -- Leading 3/4 of a nibble
          post = w .&. 15 -- Following nibble
          reg = if 0 == w .&. 1 then RAlpha else RBeta -- Lower reg
          areg = if 0 == w .&. 16 && slead /= 7 -- Higher reg (if not indirect)
                    then RAlpha
                    else RBeta
          noreg = post .>>. 1 -- Post without following reg
          addr = case slead
                 of 3 -> ALiteral
                    4 -> AAbsolute
                    5 -> AIndirect
                    6 -> AIndexed
                    7 -> if 0 == w .&. 16 then APreIndex else APostIndex
                    _ -> undefined
          ereg = case w .&. 3 -- Inc/Dec register values
                 of 0 -> EAlpha
                    1 -> EBeta
                    2 -> EStackP
                    _ -> undefined
