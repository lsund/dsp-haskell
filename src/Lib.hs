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

-- Magic numbers
kFreq = 0.12

-- Represents a downward slope function, 1.0 >= y >= `_minY` and `_minX` <= x <= `_maxX`.
-- `_power = {2,4,6...}` determines the steepness of the slope.
data SlopeParams
  = SlopeParams
      { _minX  :: Float
      , _maxX  :: Float
      , _minY  :: Float
      , _power :: Float
      }

downwardSlope :: SlopeParams -> Float -> Float
downwardSlope (SlopeParams minX maxX minY power) x | x < minX = 1.0
downwardSlope (SlopeParams minX maxX minY power) x | x > maxX = minY
downwardSlope (SlopeParams minX maxX minY power) x = ((c / maxX' * x') - c) ** power + minY
  where
    x' = x - minX
    maxX' = maxX - minX
    c = (1 - minY) ** (1 / power)

reverseDownwardSlope :: SlopeParams -> Float -> Float
reverseDownwardSlope params@(SlopeParams _ maxX _ _) sample =
  downwardSlope params (max (maxX - sample) 0)

attackFn :: Sample -> Float
attackFn = reverseDownwardSlope (SlopeParams 0 400 0.0 2)

decayFn :: Sample -> Float
decayFn = downwardSlope (SlopeParams 1200 6000 0.7 2)

sustainFn :: Sample -> Float
sustainFn = const 1.0

releaseFn :: Sample -> Float
releaseFn = downwardSlope (SlopeParams 8000 20000 0.0 2)

envelope :: Envelope
envelope = Envelope attackFn decayFn sustainFn releaseFn

frequency :: Offset -> Frequency
frequency n = pitchStandard * (2 ** (1.0 / 12.0)) ** n

amplitude :: Frequency -> Sample -> Float
amplitude hz sample = sin (reverseSample * step)
  where
    nSamples = sampleRate
    reverseSample = nSamples - sample
    w = 2 * pi * hz
    t = (1 / sampleRate) * (1 + (kFreq * downwardSlope (SlopeParams 0 8000 0.0 4) sample))
    step = w * t

signal :: Frequency -> [Pulse]
signal hz =
  map
    (\sample ->
      volume
      * amplitude hz sample
      * attack sample
      * decay sample
      * sustain sample
      * release sample)
     samples
  where
    Envelope attack decay sustain release = envelope
    samples = [1.0..sampleRate]

pulses :: Offset -> Beats -> [Pulse]
pulses n beats = signal (frequency n)

