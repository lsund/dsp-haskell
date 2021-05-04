module Lib where

import           Constants (beatDuration, pitchStandard, sampleRate, volume)
import           Types
import Data.List (zipWith4)

data Envelope = Envelope { _attack :: [Pulse], _decay :: [Pulse], _sustain :: [Pulse], _release :: [Pulse] }

-- TODO so far only linear functions. Try square functions

-- TODO overlapping signals

-- 100 ms = 4800 samples

attackDegree = 0.0015
attackCeil = 1.0

releaseDegree = 0.00008
releaseHold = 4800
releaseFloor = 0.0

decayDegree = 0.0008
decayHold = 2400
decayFloor = 0.7

freqDegree = 0.05
freqFloor = 1.0

attackFn sample = min attackCeil (sample * sample * attackDegree * attackDegree)

decayFn sample | sample < decayHold = 1
decayFn sample | sample >= decayHold && sample < releaseHold = max decayFloor (1 - (sample - decayHold) * decayDegree * decayDegree)
decayFn sample = max decayFloor (1 - decayHold * decayDegree * decayDegree)

releaseFn sample | sample < releaseHold = 1
releaseFn sample = max releaseFloor (1 - relX * relX * releaseDegree * releaseDegree)
  where relX = sample - releaseHold

-- freqFn x = 1 / (freqDegree * x + 0.2) + 1
freqFn x | x > 1000 = 1.0;
freqFn x = (0.001 * x - 1) ** 2 + 1

envelope :: Int -> Envelope
envelope length =
  Envelope
    (take length $ map attackFn [1,2 ..])
    (repeat 1.0)
    (take length $ map decayFn [1,2 ..])
    (take length $ map releaseFn [1,2 ..])

frequency :: Offset -> Frequency
frequency n = pitchStandard * (2 ** (1.0 / 12.0)) ** n

amplitude hz sample = (sin . (* step sample)) sample
  where
    step sample = (hz * freqFn sample * 2 * pi) / sampleRate

signal :: Frequency -> Seconds -> [Pulse]
signal hz duration = map (* volume) $ zipWith4 (\i j k l -> i * j * k * l) wave attack decay release
  where
    Envelope attack _ decay release = envelope (length wave)
    wave = map (amplitude hz) [0.0, 1.0 .. sampleRate * duration]

pulses :: Offset -> Beats -> [Pulse]
pulses n beats = signal (frequency n) (beats * beatDuration)

