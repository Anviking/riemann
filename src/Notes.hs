{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MagicHash     #-}
module Notes where

import           Control.Monad (msum)
import           Data.List     (sort)
import           GHC.Generics  (Generic)
import           Prelude       hiding (min)

type Note = Int

data NamedNote = C | C# | D | Eb | E | F | F# | G | G# | A | Bb | B
  deriving (Enum, Show, Generic)

inKey :: NamedNote -> Note -> NamedNote
inKey key n = toEnum ((fromEnum key + n) `mod` 12)
