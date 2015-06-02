{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import Hardware.Microcode
import GHC.Generics (Generic)
import Data.Label (set, mkLabel)
import System.Exit (exitFailure)
import System.Environment (getArgs)

data TieFlags = TieFlags { carry, equal, zero, sign, icarry, int :: Bit }
                         deriving (Show, Generic)

data TieSignal = TieSignal { _writeReg :: Bit
                           , _halted :: Bit
                           , _busOne :: Bit
                           , _busTwo :: Bit
                           , _butThree :: Bit
                           , _busFour :: Bit
                           , _rstState :: Bit
                           , _rstFlags :: Bit
                           , _wrtFlags :: Bit
                           } deriving (Show)

data TieInstruction = Halt | Move | Add deriving (Eq, Show, Generic)

mkLabel ''TieSignal

instance Flags TieFlags where
    flagBits TieFlags{..} = bit "carry" carry ++
                            bit "equal" equal ++
                            bit "zero" zero ++
                            bit "sign" sign ++
                            bit "icarry" icarry ++
                            bit "interrupt" int
instance Default TieFlags where
    def = TieFlags Low Low Low Low Low Low
instance Enumerable TieFlags
instance Instruction TieInstruction where
    instructonBits Halt = makeBits "instruction" [Low, Low]
    instructonBits Move = makeBits "instruction" [Low, High]
    instructonBits Add = makeBits "instruction" [High, Low]
instance Defaults TieInstruction where
    defs = [Halt, Move, Add]
instance Enumerable TieInstruction
instance Signal TieSignal where
    nop = TieSignal Low Low Low Low Low Low Low Low Low
    signalBits TieSignal{..} = bit "writeReg" _writeReg ++
                               bit "halted" _halted ++
                               bit "busOne" _busOne ++
                               bit "busTwo" _busTwo ++
                               bit "butThree" _butThree ++
                               bit "busFour" _busFour ++
                               bit "rstState" _rstState ++
                               bit "rstFlags" _rstFlags ++
                               bit "wrtFlags" _wrtFlags

showNested :: (Show a) => [[a]] -> String
showNested = unlines . map (unlines . map show)

regWrite :: Bit -> Directive TieSignal
regWrite d = cmd $ set writeReg d

process :: TieInstruction -> Signals TieFlags TieSignal
process Halt = do
    tickF $ \TieFlags{carry} -> do nothing
                                   regWrite carry
    tick nothing
process _ = tick nothing

tieConf :: RomConfig
tieConf = RomConfig 2 10 [Flags 6, State 2, Instruction 2]

failError :: String -> IO ()
failError str = putStrLn str >> exitFailure

generateBinary :: [String] -> IO ()
generateBinary (method:filen:_)
    | method == "binary"  = putStrLn "Generaring binary ROMs" >> exportAsFile filen rom
    | method == "logisim" = putStrLn "Generaring logisim ROMS" >> exportAsLogisim filen rom
    | otherwise           = failError $ "Unknown method " ++ method
    where rom = makeRom tieConf process
generateBinary _ = failError "Usage: <logisim|binary> <name>"

main :: IO ()
main = getArgs >>= generateBinary >> putStrLn "Done."
          

