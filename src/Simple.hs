{-# LANGUAGE MagicHash #-}

module Simple
  (
  -- * Minor chords
    i
  , i#
  , ii
  , iii
  , iv
  , v
  , vi
  , vii
  --- * Major chords
  , NTriad (I, I#, II, II#, III, IV, V, VI, VII)
  -- * Diminished chords
  , iidim
  -- * Conversions
  , toN
  )
  where

import           Notes
import           Prelude hiding (min)
import           Triad

-- TODO: What scale / mode am I in? This is probably suboptimal.

data NTriad = I | I# | II | II# | III | III# | IV | IV# | V | VI | VII | NMinor NTriad | NDim NTriad

toN (NMinor I)    = Minor 0
toN (NMinor I#)   = Minor 1
toN (NMinor II)   = Minor 2
toN (NMinor III)  = Minor 3
toN (NMinor III#) = Minor 4
toN (NMinor IV)   = Minor 5
toN (NMinor V)    = Minor 7
toN (NMinor VI)   = Minor 8
toN (NMinor VII)  = Minor 10
toN I             = Major 0
toN I#            = Major 1
toN II            = Major 2
toN III           = Major 3
toN III#          = Major 4
toN IV            = Major 5
toN IV#           = Major 6
toN V             = Major 7
toN VI            = Major 8
toN VII           = Major 10
toN (NDim I)      = Minor 0
toN (NDim I#)     = Minor 1
toN (NDim II)     = Minor 2
toN (NDim III)    = Minor 3
toN (NDim III#)   = Minor 4
toN (NDim IV)     = Minor 5
toN (NDim V)      = Minor 7
toN (NDim VI)     = Minor 8
toN (NDim VII)    = Minor 10

i = NMinor I
i# = NMinor I#
ii = NMinor II
iii = NMinor III
iv = NMinor IV
v = NMinor V
vi = NMinor VI
vii = NMinor VII

iidim = NDim II
