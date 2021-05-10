module Constants where

import Types

volume :: Float
volume = 1.0

-- samples / second
sampleRate :: Float
sampleRate = 48000.0

fsharp0 = 23.12
fsharp1 = 46.26
f2 = 87.31

pitchStandard :: Frequency
pitchStandard = fsharp1

bpm :: Beats
bpm = 20.0

beatDuration :: Seconds
beatDuration = 60.0 / bpm

outputFilePath :: FilePath
outputFilePath = "./data/output2.bin"

plotFilePath :: FilePath
plotFilePath = "./data/plot.png"
