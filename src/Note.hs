module Note where

import           Lib       (frequency, pulses)
import           Types

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

note AMajor = pulses 0 1
note ASharp = pulses 1 1
note BMajor = pulses 2 1
note CMajor = pulses 3 1
note CSharp = pulses 4 1
note DMajor = pulses 5 1
note DSharp = pulses 6 1
note EMajor = pulses 7 1
note FMajor = pulses 8 1
note FSharp = pulses 9 1
note GMajor = pulses 10 1
note GSharp = pulses 11 1

amajor = zipWith3 (\x y z -> x + y + z) (note AMajor) (note BMajor) (note CSharp)
