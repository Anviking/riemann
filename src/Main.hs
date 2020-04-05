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
import           Progression
import           Riemann
import           Simple
import           System.MIDI
import           System.MIDI.Base
import           System.MIDI.Utility


main :: IO ()
main = do
 --mapM_ print $ map (transform (map rotate2)) [dm, gm, aM]
 let chords' = (transform' (toN i) swap2) (map toN circle)
 withMidiSession $ \c -> do
   mapM_ (playTriad c) (chords' <> [toN i])


--
--
--

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
