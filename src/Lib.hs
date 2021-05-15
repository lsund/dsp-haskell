module Lib where

import           Constants    (pitchStandard, sampleRate, volume)
import           Data.Complex
import           Envelope
import           Types

-- Frequency decrease magic number. Frequency of note starts at 200% and quickly decreases
-- to 100% to simulate the initial hit. Increasing `kFreq` increases the rate
-- of decrease.
kFreq = 0.0015

-- https://pages.mtu.edu/~suits/NoteFreqCalcs.html
frequency :: HalfNotes -> Frequency
frequency n = pitchStandard * (2 ** (1.0 / 12.0)) ** n

-- https://dsp.stackexchange.com/questions/2051/why-does-a-wave-continuously-decreasing-in-frequency-start-increasing-its-freque
--
--
-- TODO Smoothly go between two frequencies
--
-- https://dsp.stackexchange.com/questions/971/how-to-create-a-sine-wave-generator-that-can-smoothly-transition-between-frequen?noredirect=1&lq=1
--
-- https://github.com/rmichela/Acoustico/blob/194945595e78d1e0d1a46d9ffde03fc8a7212d38/phasor.go
--
--


data TrigGenerator
  = TrigGenerator
      { _time          :: Double
      , _stabilityTime :: Double
      , phasor         :: Complex Double
      , sampleRate     :: Double
      }


wave :: Double -> Frequency -> TimeStamp -> Double
wave amp hz t = volume * sin (2 * pi * t * hz / sampleRate)

signal :: Frequency -> [Pulse]
signal hz =
  map
    (\t ->
      wave volume hz t
      * attack t
      * decay t
      * sustain t
      * release t)
     timestamps
  where
    Envelope attack decay sustain release = makeEnvelope
    timestamps = [1.0..sampleRate]

pulses :: HalfNotes -> Beats -> [Pulse]
pulses n beats = signal (frequency n)

