{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Triad where

import           Data.List    (sort)
import           GHC.Generics (Generic)
import           Prelude      hiding (min)

import           Notes

--  Note: @Triad@ allows the root to be placed in different octaves, but the
--  show instance ignores the octave. Yup. This is weird.
data Triad = Minor Note | Major Note | Dim Note
  deriving (Eq, Generic)

root :: Triad -> Note
root (Minor r) = r
root (Major r) = r
root (Dim r)   = r

instance Show Triad where
  show (Minor n) = show (inKey D n) <> "m"
  show (Major n) = show (inKey D n)
  show (Dim n)   = show (inKey D n) <> "°"

instance Read Triad where
  readsPrec d str =
    let
      ((nn::NamedNote, r):_) = readsPrec d str
      (q,r') = readQuality r
    in [(q (fromEnum nn - 2), r')]
    where
      readQuality ('m':xs) = (Minor, xs)
      readQuality ('°':xs) = (Dim, xs)
      readQuality xs       = (Major, xs)

-- | Construct a /major/ triad from a given root
maj :: Note -> [Note]
maj n = [n, n + 4, n + 7]

-- | Construct a /minor/ triad from a given root
min :: Note -> [Note]
min n = [n, n + 3, n + 7]

-- | Construct a /diminished/ triad from a given root
dim :: Note -> [Note]
dim n = [n, n + 3, n + 6]

toNotes (Minor n) = min n
toNotes (Major n) = maj n
toNotes (Dim n)   = dim n


-- | Normalize the @Triad@ to a single octave.
normalize = transposeTriad (`mod` 12)

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
