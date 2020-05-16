{-# LANGUAGE MagicHash #-}
module Notes where

import           Control.Monad (msum)
import           Data.List     (sort)
import           Prelude       hiding (min)

type Note = Int

data NamedNote = C | C# | D | Eb | E | F | F# | G | G# | A | Bb | B
  deriving (Enum, Show)

inKey :: NamedNote -> Note -> NamedNote
inKey key n = toEnum ((fromEnum key + n) `mod` 12)

maj :: Note -> [Note]
maj n = [n, n + 4, n + 7]

min :: Note -> [Note]
min n = [n, n + 3, n + 7]


toNotes (Minor n) = min n
toNotes (Major n) = maj n


data Triad = Minor Note | Major Note | Dim Note
  deriving Eq

instance Show Triad where
  show (Minor n) = show (inKey D n) <> "m"
  show (Major n) = show (inKey D n)
  show (Dim n)   = show (inKey D n) <> "°"


transposeTriad :: (Note -> Note) -> Triad -> Triad
transposeTriad f (Minor r) = Minor (f r)
transposeTriad f (Major r) = Major (f r)
transposeTriad f (Dim r)   = Dim (f r)

rel x root = (x - root) `mod` 12


-- | Primitive chord classificaion
classify :: [Note] -> Maybe Triad
classify = go []
  where
    go :: [Note] -> [Note] -> Maybe Triad
    go triedRoots (root:tail) =
      let
        notes = triedRoots ++ tail
        intervals = sort $ map (`rel` root) notes
      in case intervals of
        (4:7:_) -> Just $ Major root
        (3:7:_) -> Just $ Minor root
        (3:6:_) -> Just $ Dim root
        _       -> go (root:triedRoots) tail
    go _ [] = Nothing


