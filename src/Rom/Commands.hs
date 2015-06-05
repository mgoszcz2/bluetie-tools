module Rom.Commands where

import Hardware.Microcode
import Types.Ucode

chg :: (Flags f, Signal i) => (i -> i) -> Signals f i
chg = tick . cmd

-- | Called at the end of each instruction to prepare for the next one
-- In one cycle try to reset state and while doing set vBus to RAM ahead of time and IncPC now
-- it's not the job of the start cycle to do so (since prev state is unknown)
-- Means that each instruction should leave PC at last byte it used
allEnd :: Signals TieFlags TieSignal
allEnd = chg $ \s -> s {ucodeCtrl = ucodeBits URstState, vBusStd = valueBits RamVal, incPc = High}

-- | Called at the start of all instructions (this could start from unkown state), most important
-- Rising next cycle: Writes instruction into memory - we are set
allStart :: Signals TieFlags TieSignal
allStart = chg $ \s -> s {ucodeCtrl = ucodeBits UWeInstReg, vBusStd = valueBits RamVal}
