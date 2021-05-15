module Constants where

import Types

volume :: Double
volume = 1.0

-- samples / second
sampleRate :: Double
sampleRate = 48000.0

c0 = 16.35

pitchStandard :: Frequency
pitchStandard = c0

bpm :: Beats
bpm = 20.0

beatDuration :: Seconds
beatDuration = 60.0 / bpm

outputFilePath :: FilePath
outputFilePath = "./data/output.raw"

plotFilePath :: FilePath
plotFilePath = "./data/plot.png"
