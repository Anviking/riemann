module Harmony.Riemann where

import           Data.List
import           Data.Maybe (catMaybes, fromJust)
import           Prelude    hiding (min)

import           Notes
import           Triad

-- | The P transformation exchanges a triad for its Parallel. In a Major Triad
-- move the third down a semitone (C major to C minor), in a Minor Triad move
-- the third up a semitone (C minor to C major)
p :: Triad -> Triad
p (Minor n) = Major n
p (Major n) = Minor n

-- | The R transformation exchanges a triad for its Relative. In a Major Triad
-- move the fifth up a tone (C major to A minor), in a Minor Triad move the
-- root down a tone (A minor to C major)
r :: Triad -> Triad
r (Minor n) = Major (n + 3)
r (Major n) = Minor (n - 3)

-- | The L transformation exchanges a triad for its Leading-Tone Exchange. In a
-- Major Triad the root moves down by a semitone (C major to E minor), in a
-- Minor Triad the fifth moves up by a semitone (E minor to C major)
l :: Triad -> Triad
l (Minor n) = Major (n - 4)
l (Major n) = Minor (n + 4)

-- | The N (or Nebenverwandt) relation exchanges a major triad for its minor
-- subdominant, and a minor triad for its major dominant (C major and F minor).
-- The "N" transformation can be obtained by applying R, L, and P successively
n = p . l . r

-- | The S (or Slide) relation exchanges two triads that share a third (C major
-- and C? minor); it can be obtained by applying L, P, and R successively in
-- that order
s = r . p . l

-- | The H relation (LPL) exchanges a triad for its hexatonic pole (C major and
-- A♭ minor)
h = l . p . l

toNotes (Minor n) = min n
toNotes (Major n) = maj n


fromT xs = fromT' (reverse xs)
fromT' ('p':xs) = p . fromT xs
fromT' ('r':xs) = r . fromT xs
fromT' ('l':xs) = l . fromT xs
fromT' ('n':xs) = n . fromT xs
fromT' ('s':xs) = s . fromT xs
fromT' ('h':xs) = h . fromT xs
fromT' ""       = id

data Op = P | R | L
  deriving Show

f :: Triad -> [Op] -> Triad
f root = foldl' f' root
  where
  f' x P = p x
  f' x R = r x
  f' x L = l x

g :: Triad -> Triad -> [Op]
g a b = fromJust $ foldl' step Nothing combinations
 where
  step :: (Maybe [Op]) -> [Op] -> Maybe [Op]
  step prev c =
    let i' = norm $ f a c
    in if i' == (norm b) then Just $ go prev c else prev

  go (Just c1) c2 = if length c2 < length c1 then c2 else c1
  go Nothing c2   = c2

  norm (Minor n) = Minor $ 60 + (n `mod` 12)
  norm (Major n) = Major $ 60 + (n `mod` 12)


  combinations :: [[Op]]
  combinations = do
    a <- allOp
    b <- allOp
    c <- allOp
    d <- allOp
    e <- allOp
    f <- allOp
    return $ catMaybes [a,b,c,d, e, f]
  allOp = [Just P, Just R, Just L, Nothing]


transform :: Triad -> ([Op] -> [Op]) -> Triad -> Triad
transform center tr = f center . tr . g center

transform' :: Triad -> (Op -> Op) -> [Triad] -> [Triad]
transform' center f = map (transform center (map f))

transformRelative' :: Triad -> (Op -> Op) -> [Triad] -> [Triad]
transformRelative' center f (x:xs) =
  let x' = transform center (map f) x
  in x' : transformRelative' x' f xs
transformRelative' _ _ _ = []

rotate1 :: Op -> Op
rotate1 P = R
rotate1 R = L
rotate1 L = P

rotate2 :: Op -> Op
rotate2 P = L
rotate2 R = P
rotate2 L = R

swap1 :: Op -> Op
swap1 P = P
swap1 R = L
swap1 L = R

swap2 :: Op -> Op
swap2 P = R
swap2 R = P
swap2 L = L

weird1 :: Op -> Op
weird1 P = R
weird1 R = R
weird1 L = L

weird2 :: Op -> Op
weird2 P = L
weird2 R = R
weird2 L = L

weird3 :: Op -> Op
weird3 P = R
weird3 R = L
weird3 L = R
