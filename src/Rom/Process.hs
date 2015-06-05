{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module Rom.Process where

import Types.Ucode
import Rom.Commands
import Types.Instruction
import Hardware.Microcode

process :: TieInstruction -> Signals TieFlags TieSignal
process (Inc r) = do allStart
                     tick $ aluCmd FInc >> outReg r >> weReg r
                     allEnd
process (Dec r) = do allStart
                     tick $ aluCmd FInc >> outReg r >> weReg r
                     allEnd
process Nop = allStart >> allEnd
process _   = allStart >> allEnd
