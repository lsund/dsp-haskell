module Note where

import           Lib   (pulses)
import           Types

data Note = AMajor | ASharp | BMajor | CMajor | CSharp | DMajor | DSharp | EMajor | FMajor | FSharp | GMajor | GSharp deriving
    ( Enum
    , Show
    )

toPulses :: Note -> Double -> [Pulse]
toPulses note oct = pulses ((12 * oct) + fromIntegral (fromEnum note)) 1
