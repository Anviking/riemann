-- | https://en.wikipedia.org/wiki/Axis_system
module Harmony.Axis where

import           Notes (Note)

-- | Representing a chords' function according to functional harmony.
data Function
  = T -- ^ Tonic (e.g. Dm)
  | S -- ^ Subdominant (e.g. Gm)
  | D -- ^ Dominant (e.g. A7)
  deriving (Show, Eq)

classify
  :: Note -- ^ Root of the key to classify according to
  -> Note -- ^ Note to classify
  -> Function
classify r n = case (n - r) `mod` 3 of
  0 -> T
  1 -> D
  2 -> S
