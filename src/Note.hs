module Note where

import Types
import           Lib       (pulses)

data Note = AMajor
          | ASharp
          | BMajor
          | CMajor
          | CSharp
          | DMajor
          | DSharp
          | EMajor
          | FMajor
          | FSharp
          | GMajor
          | GSharp
          deriving (Show)

note :: Note -> Double -> [Pulse]
note CMajor oct = pulses  0 1
note CSharp oct = pulses  1 1
note DMajor oct = pulses  2 1
note DSharp oct = pulses  3 1
note EMajor oct = pulses  4 1
note FMajor oct = pulses  5 1
note FSharp oct = pulses  6 1
note GMajor oct = pulses  7 1
note GSharp oct = pulses  8 1
note AMajor oct = pulses  9 1
note ASharp oct = pulses 10 1
note BMajor oct = pulses 11 1

-- amajor = zipWith3 (\x y z -> x + y + z) (note AMajor) (note BMajor) (note CSharp)
