module Envelope where

import Types
import Constants

data Envelope
  = Envelope
      { _attack  :: TimeStamp -> Pulse
      , _decay   :: TimeStamp -> Pulse
      , _sustain :: TimeStamp -> Pulse
      , _release :: TimeStamp -> Pulse
      }

data SlopeParams
  = SlopeParams
      { _minX   :: Double
      , _maxX   :: Double
      , _limitY :: Double
      , _power  :: Double
      }

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

makeEnvelope :: Envelope
makeEnvelope =
  Envelope
    (upSlope   (SlopeParams 0    800  1.0 2))
    (downSlope (SlopeParams 1200 6000 0.7 2))
    (const 1.0)
    (downSlope (SlopeParams 8000 50000 0.0 2))
