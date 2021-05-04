module Constants where

import Types

volume :: Float
volume = 0.4

-- samples / second
sampleRate :: Samples
sampleRate = 48000.0

fsharp0 = 23.12
fsharp1 = 46.26
f2 = 87.31

pitchStandard :: Frequency
pitchStandard = fsharp0

bpm :: Beats
bpm = 100.0

beatDuration :: Seconds
beatDuration = 60.0 / bpm

outputFilePath :: FilePath
outputFilePath = "data/output2.bin"
-- outputFilePath = "data/output.bin"

