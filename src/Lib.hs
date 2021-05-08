module Lib where

import           Constants (beatDuration, pitchStandard, sampleRate, volume)
import           Types
import Data.List (zipWith4)

data Envelope = Envelope { _attack :: [Pulse], _decay :: [Pulse], _sustain :: [Pulse], _release :: [Pulse] }

-- TODO so far only linear functions. Try square functions

-- TODO overlapping signals

-- 100 ms = 4800 samples

-- 0 <= stretch <= 1
stretch = 0.5

attackDegree = 0.0015
attackCeil = 1.0
attackHold = 1200

decayDegree = 0.0008
decayFloor = 0.7
decayHold = 6000

releaseDegree = 0.00006
releaseFloor = 0.0

freqDegree = 0.0000000017
freqFloor = 1.0

attackFn sample = min attackCeil (sample * sample * attackDegree * attackDegree)

decayFn sample | sample <= attackHold = 1.0
decayFn sample | sample > attackHold && sample <= decayHold = max decayFloor (1 - relX * decayDegree)
  where relX = sample - attackHold
decayFn sample = decayFloor

releaseFn sample | sample <= decayHold = 1
releaseFn sample = max releaseFloor (1 - relX * relX * releaseDegree * releaseDegree)
  where relX = sample - decayHold

envelope :: Int -> Envelope
envelope length =
  Envelope
    (take length $ map attackFn [1,2 ..])
    (repeat 1.0)
    (take length $ map decayFn [1,2 ..])
    (take length $ map releaseFn [1,2 ..])

frequency :: Offset -> Frequency
frequency n = pitchStandard * (2 ** (1.0 / 12.0)) ** n

point hz sample = sin (sample * step)
  where
    w = 2 * pi * hz
    t = sample * freqDegree
    step = w * t + (1 / sampleRate) * ((t ** 2) / 2)


signal :: Frequency -> [Pulse]
signal hz = map (* volume) $ zipWith4 (\i j k l -> i * j * k * l) wave attack decay release
  where
    Envelope attack _ decay release = envelope (length wave)
    samples = sampleRate  * stretch
    wave = map (point hz) [samples, pred samples.. 0]

pulses :: Offset -> Beats -> [Pulse]
pulses n beats = signal (frequency n)

