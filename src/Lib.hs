module Lib where

import           Constants (pitchStandard, sampleRate, volume)
import           Types

data Envelope
  = Envelope
      { _attack  :: TimeStamp -> Pulse
      , _decay   :: TimeStamp -> Pulse
      , _sustain :: TimeStamp -> Pulse
      , _release :: TimeStamp -> Pulse
      }

data SlopeParams
  = SlopeParams
      { _minX  :: Double
      , _maxX  :: Double
      , _limitY  :: Double
      , _power :: Double
      }

-- Frequency decrease magic number. Frequency of note starts at 200% and quickly decreases
-- to 100% to simulate the initial hit. Increasing `kFreq` increases the rate
-- of decrease.
kFreq = 0.12

-- https://www.desmos.com/calculator

-- Represents a downward slope , or a left part of a function similar to
-- x^2.
-- More specifically, the section that spans 1.0 >= y >= `minY` and `minX` <= x <= `maxX`.
-- `power \in {2,4,6...}` determines the initial steepness of the slope.
downSlope :: SlopeParams -> Double -> Double
downSlope (SlopeParams minX maxX minY power) x | x < minX = 1.0
downSlope (SlopeParams minX maxX minY power) x | x > maxX = minY
downSlope (SlopeParams minX maxX minY power) x = ((c / maxX' * x') - c) ** power + minY
  where
    x'    = x - minX
    maxX' = maxX - minX
    c     = (1 - minY) ** (1 / power)

-- See downward slope, but for -x^2
upSlope :: SlopeParams -> Double -> Double
upSlope (SlopeParams minX maxX maxY power) x | x < minX = 0.0
upSlope (SlopeParams minX maxX maxY power) x | x > maxX = maxY
upSlope (SlopeParams minX maxX maxY power) x = -((c / maxX' * x') - c) ** power + maxY
  where
    x'    = x - minX
    maxX' = maxX - minX
    c     = maxY ** (1 / power)

attackFn :: TimeStamp -> Double
attackFn = upSlope (SlopeParams 0 800 1.0 2)

decayFn :: TimeStamp -> Double
decayFn = downSlope (SlopeParams 1200 6000 0.7 2)

sustainFn :: TimeStamp -> Double
sustainFn = const 1.0

releaseFn :: TimeStamp -> Double
releaseFn = downSlope (SlopeParams 8000 20000 0.0 2)

envelope :: Envelope
envelope = Envelope attackFn decayFn sustainFn releaseFn

frequency :: Offset -> Frequency
frequency n = pitchStandard * (2 ** (1.0 / 12.0)) ** n

amplitude :: Frequency -> TimeStamp -> Double
amplitude hz sample = sin (reverseSample * angle)
  where
    nSamples = sampleRate
    reverseSample = nSamples - sample
    w = 2 * pi * hz
    t = (1 / sampleRate) * (1 + (kFreq * downSlope (SlopeParams 0 8000 0.0 4) sample))
    angle = w * t

signal :: Frequency -> [Pulse]
signal hz =
  map
    (\t ->
      volume
      * amplitude hz t
      * attack t
      * decay t
      * sustain t
      * release t)
     timestamps
  where
    Envelope attack decay sustain release = envelope
    timestamps = [1.0..sampleRate]

pulses :: Offset -> Beats -> [Pulse]
pulses n beats = signal (frequency n)

