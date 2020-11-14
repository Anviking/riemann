-- Can we do something with voice leading?
--
-- Maybe interesting:
-- https://hal.archives-ouvertes.fr/hal-01218169/document
module Voice.Leading where

import           Notes (Note)
import           Triad (Triad (..), toNotes)

-- | "Differentiate" a list of notes to obtain how the melody moves.
--
-- >>> delta [0,5,5,6]
-- [5,0,1]
delta :: [Note] -> [Int]
delta ns = zipWith (flip (-)) ns (tail ns)

-- | Like @delta@, but for Triads
--
-- TODO: This is pretty boring since @Triad@ assumes root position. I suspect there's some fancy
-- abstract algebra could be applied here though.
--
-- >>> deltaTriad  $ map read $ ["Dm", "Em"]
-- [[2],[2],[2]]
deltaTriad :: [Triad] -> [[Int]]
deltaTriad ts = map delta $ transpose $ map toNotes ts
  where
    transpose:: [[a]]->[[a]]
    transpose ([]:_) = []
    transpose x      = map head x : transpose (map tail x)

tupToList :: (a,a,a) -> [a]
tupToList (a,b,c) = [a,b,c]

-- Naming from https://hal.archives-ouvertes.fr/hal-01218169/document
data Motion
  = Similar -- ^ when the voices move in the same direction
  | Contrary -- ^ when the voices move in opposite directions
  | Oblique -- ^ when only one voice is moving
  | Same -- ^ When nothing changes (does it make sense to keep this constructor?
  deriving (Eq, Show)

classify :: [Int] -> Motion
classify xs =
  let
    up = length $ filter (> 0) xs
    down = length $ filter (< 0) xs
    same = length $ filter (== 0) xs
  in
    -- Not sure this makes sense
    case (up > 0, down > 0, same > 0) of
      (False,False, _)             -> Same
      (True, True, _)              -> Contrary
      (True, False, _) | up >= 2   -> Similar
      (True, False, _) | otherwise -> Oblique
      (False, True, _) | down >= 2 -> Similar
      (False, True, _) | otherwise -> Oblique
