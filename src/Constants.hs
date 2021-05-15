module Constants where

import Types

volume :: Double
volume = 1.0

-- samples / second
sampleRate :: Double
sampleRate = 48000.0

a4 :: Frequency
a4 = 440.00

pitchStandard :: Frequency
pitchStandard = a4

bpm :: Beats
bpm = 20.0

beatDuration :: Seconds
beatDuration = 60.0 / bpm

outputFilePath :: FilePath
outputFilePath = "./data/output.raw"

plotFilePath :: FilePath
plotFilePath = "./data/plot.png"
