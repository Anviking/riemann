{-# LANGUAGE MagicHash #-}
module Notes where


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


