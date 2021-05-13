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

attackDegree = 0.0025
attackCeil = 1.0
attackHold = 1200

decayDegree = 0.0002
decayFloor = 0.7
decayHold = 6000

releaseDegree = 0.00006
releaseFloor = 0.0

decreasing :: Float -> Float -> Float
decreasing k x = ((1 / k * x) - sqrt 1) ** 2

-- Represents a decreasing slope for decreasing samples.
--
-- If min <= x <= max then return 0 <= y <= 1 else return 0
--
-- For max, return 1, for min return 0
-- between, return a number representing a downward slope
slopeBetween :: Float -> Float -> Sample -> Float
slopeBetween max min x | x < min = 0.0
slopeBetween max min x = decreasing (max - min) (max - x)

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
envelope = Envelope attackFn decayFn (const 1.0) releaseFn

frequency :: Offset -> Frequency
frequency n = pitchStandard * (2 ** (1.0 / 12.0)) ** n

amplitude :: Frequency -> Sample -> Float
amplitude hz sample = sin (reverseSample * step)
  where
    nSamples = sampleRate
    reverseSample = nSamples - sample
    w = 2 * pi * hz
    t = (1 / sampleRate) * (1 + (0.12 * slopeBetween sampleRate (sampleRate - 5000) reverseSample))
    step = w * t

signal :: Frequency -> [Pulse]
signal hz =
  map (\sample -> volume * amplitude hz sample * attack sample * decay sample * 1.0 * release sample) samples
  where
    Envelope attack decay _ release = envelope
    samples = [1.0..sampleRate]

pulses :: Offset -> Beats -> [Pulse]
pulses n beats = signal (frequency n)

