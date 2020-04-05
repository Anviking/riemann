{-# LANGUAGE MagicHash #-}
module Main where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Writer
import           Data.Function              ((&))
import           Data.List
import           Data.Maybe
import           Prelude                    hiding (min)
import           System.MIDI
import           System.MIDI.Base
import           System.MIDI.Utility

type Note = Int

data NamedNote = C | C# | D | Eb | E | F | F# | G | G# | A | Bb | B
  deriving (Enum, Show)

inKey :: NamedNote -> Note -> NamedNote
inKey key n = toEnum ((fromEnum key + n) `mod` 12)

maj :: Note -> [Note]
maj n = [n, n + 4, n + 7]

min :: Note -> [Note]
min n = [n, n + 3, n + 7]

data Triad = Minor Note | Major Note
  deriving Eq

instance Show Triad where
  show (Minor n) = show (inKey D n) <> "m"
  show (Major n) = show (inKey D n)

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
    let i' = f a c
    in if i' == b then Just $ go prev c else prev

  go (Just c1) c2 = if length c2 < length c1 then c2 else c1
  go Nothing c2   = c2


  combinations :: [[Op]]
  combinations = do
    a <- allOp
    b <- allOp
    c <- allOp
    d <- allOp
    e <- allOp
    return $ catMaybes [a,b,c,d, e]
  allOp = [Just P, Just R, Just L, Nothing]


transform :: ([Op] -> [Op]) -> Triad -> Triad
transform tr = f center . tr . g center
  where
    center = Minor 0

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

main :: IO ()
main = do
 let gm = r $ l $ Minor 0
 let dm = Minor 0
 let aM = Major 7
 --let chords = [Minor 0, Major 2, Major 1, Major 0]
 --let chords = [dm, gm, aM]
 let chords = [Minor 0, Major 10, Major 8, Major 7]
 --mapM_ print $ map (transform (map rotate2)) [dm, gm, aM]
 let chords' = map (transform (map rotate1)) (chords)

 withMidiSession $ \c -> do
   let i = Minor 0
   mapM_ (playTriad c) chords'

playTriad c t = do
  print t
  playNotes c (toNotes t)
playNotes c ns = do
  let home = 60
  forM_ ns $ \n ->
    send c $ MidiMessage 0 (NoteOn (n + home) 90)
  threadDelay 1500000
  forM_ ns $ \n ->
    send c $ MidiMessage 0 (NoteOff (n + home) 90)

withMidiSession :: (Connection -> IO ()) -> IO ()
withMidiSession f = do
  target <- (!! 0) <$> enumerateDestinations
  c <- openDestination target
  forM_ [0..100]$ \n ->
    send c $ MidiMessage 0 (NoteOff n 90)
  start c
  f c
  stop c
