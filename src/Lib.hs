module Lib where

import           Constants (pitchStandard, sampleRate, volume)
import           Types

data Envelope
  = Envelope
      { _attack  :: Sample -> Pulse
      , _decay   :: Sample -> Pulse
      , _sustain :: Sample -> Pulse
      , _release :: Sample -> Pulse
      }

e = 2.71828

-- https://www.desmos.com/calculator

-- TODO smooth decay and release using decreasing as well

-- 0 <= stretch <= 1
stretch = 0.5

attackDegree = 0.0025
attackCeil = 1.0
attackHold = 1200

decayDegree = 0.0002
decayFloor = 0.7
decayHold = 6000

releaseDegree = 0.00006
releaseFloor = 0.0

-- Quadratic decreasing function between max and min. If min <= x <= max then
-- return 0 <= y <= 1 else return x
decreasing max min x | x < min = 0.0
decreasing max min x = go (max - x)
  where
    go x = ((1 / (max - min)) * x - sqrt 1) ** 2

attackFn :: Sample -> Float
attackFn sample = min attackCeil (sample * sample * attackDegree * attackDegree)

decayFn :: Sample -> Float
decayFn sample | sample <= attackHold = 1.0
decayFn sample | sample > attackHold && sample <= decayHold = max decayFloor (1 - relX * decayDegree)
  where relX = sample - attackHold
decayFn sample = decayFloor

releaseFn :: Sample -> Float
releaseFn sample | sample <= decayHold = 1
releaseFn sample = max releaseFloor (1 - relX * relX * releaseDegree * releaseDegree)
  where relX = sample - decayHold

envelope :: Envelope
envelope =
  Envelope attackFn decayFn (const 1.0) releaseFn
  where samples = [1..];

frequency :: Offset -> Frequency
frequency n = pitchStandard * (2 ** (1.0 / 12.0)) ** n

amplitude :: Frequency -> Sample -> Float
amplitude hz sample = sin (sample * step)
  where
    w = 2 * pi * hz
    t = (1 / sampleRate) * (1 + (0.12 * decreasing sampleRate (sampleRate - 5000) sample))
    step = w * t

signal :: Frequency -> [Pulse]
signal hz =
  zipWith (curry ((*volume) .  (\(sample, reverseSample) -> amplitude hz reverseSample * attack sample * decay sample * 1.0 * release sample)))
    samples
    reverseSamples
  where
    Envelope attack decay sustain release = envelope
    nSamples = sampleRate
    samples = [1.0..sampleRate]
    reverseSamples = [nSamples, pred nSamples.. 0]

pulses :: Offset -> Beats -> [Pulse]
pulses n beats = signal (frequency n)

