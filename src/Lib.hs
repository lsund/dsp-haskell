module Lib where

import           Constants (beatDuration, pitchStandard, sampleRate, volume)
import           Types
import Data.List (zipWith4)

data Envelope = Envelope { _attack :: [Pulse], _decay :: [Pulse], _sustain :: [Pulse], _release :: [Pulse] }

-- 100 ms = 4800 samples
releaseHold = 4800 * 3

kAttack = 0.035
kDecay = 0.0002
kRelease = 0.0002

decayHold = 2400
decayFloor = 0.5

kFreq = 0.00002
freqCeil = 1.5
freqFloor = 1.0

decayFn sample | sample < decayHold = 1
decayFn sample = 1 - (sample - decayHold) * kDecay

releaseFn sample | sample < releaseHold = 1
releaseFn sample = 1 - (sample - releaseHold) * kRelease

freqFn sample | sample < decayHold = freqCeil
freqFn sample = freqCeil - (sample - decayHold) * kFreq

envelope :: Int -> Envelope
envelope length =
  Envelope
    (take length $ map (min 1.0 . (* kAttack)) [1,2 ..])
    (repeat 1.0)
    (take length $ map (max decayFloor . decayFn) [1,2 ..])
    (take length $ map (max 0.0 . releaseFn) [1,2 ..])

frequency :: Offset -> Frequency
frequency n = pitchStandard * (2 ** (1.0 / 12.0)) ** n

amplitude hz sample = (sin . (* step sample)) sample
  where
    step sample = ((hz * 2 * pi) / sampleRate) * max freqFloor (freqFn sample)

signal :: Frequency -> Seconds -> [Pulse]
signal hz duration = map (* volume) $ zipWith4 (\i j k l -> i * j * k * l) wave attack decay release
  where
    Envelope attack _ decay release = envelope (length wave)
    wave = map (amplitude hz) [0.0, 1.0 .. sampleRate * duration]

pulses :: Offset -> Beats -> [Pulse]
pulses n beats = signal (frequency n) (beats * beatDuration)

