module Main where

import Assembly
import Rom.Process
import Hardware.Microcode

import System.Exit (exitFailure)
import System.Environment (getArgs)

showNested :: (Show a) => [[a]] -> String
showNested = unlines . map (unlines . map show)

tieConf :: RomConfig
tieConf = RomConfig 3 16 [State 3, Instruction 8, Flags 5]

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

