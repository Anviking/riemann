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

instance Read NamedNote where
  readsPrec _ ('C':'#':xs) = [(C#, xs)]
  readsPrec _ ('C':xs)     = [(C, xs)]
  readsPrec _ ('D':xs)     = [(D, xs)]
  readsPrec _ ('E':'b':xs) = [(Eb, xs)]
  readsPrec _ ('E':xs)     = [(E, xs)]
  readsPrec _ ('F':'#':xs) = [(F#, xs)]
  readsPrec _ ('F':xs)     = [(F, xs)]
  readsPrec _ ('G':'#':xs) = [(G#, xs)]
  readsPrec _ ('G':xs)     = [(G, xs)]
  readsPrec _ ('A':xs)     = [(A, xs)]
  readsPrec _ ('B':'b':xs) = [(Bb, xs)]
  readsPrec _ ('B':xs)     = [(B, xs)]

inKey :: NamedNote -> Note -> NamedNote
inKey key n = toEnum ((fromEnum key + n) `mod` 12)
