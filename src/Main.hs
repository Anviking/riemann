{-# LANGUAGE MagicHash #-}
module Main where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Writer
import           Data.Function              ((&))
import           Data.List
import           Data.Maybe
import           Fmt
import           Negative
import           Notes
import           Prelude                    hiding (min)
import           Progression
import           Riemann
import           Simple
--import           System.MIDI
--import           System.MIDI.Base
--import           System.MIDI.Utility

import qualified Data.Text.IO               as TIO

main =
  print $ map (transposeTriad (+7) . fromJust . invertTriad 0 . toN) circle

--main :: IO ()
--main = do
--  section circle "circle-esque"
--  section northKorea "north korea"
--  section neapolitan "neapolitan"
--  section wc2Human "WC2 Human 1"
--
--  where
--    section :: [NTriad] -> String -> IO ()
--    section prog name = do
--      putStrLn $ "## " <> name <> ":"
--      exp "" id prog
--      exp "Rotate1" (transform' (toN i) rotate1) prog
--      exp "Rotate2" (transform' (toN i) rotate2) prog
--      exp "Swap1" (transform' (toN i) swap1) prog
--      exp "Swap2" (transform' (toN i) swap2) prog
--      putStrLn ""
--
--    exp str f prog =
--      fmtLn $ descF str <> (chordsF . f . map toN $ prog)
--
--    descF = padRightF 10 ' '
--    chordF c = padRightF 4 ' ' (build $ show c)
--    chordsF = mconcat . map chordF

--main :: IO ()
--main = do
-- --mapM_ print $ map (transform (map rotate2)) [dm, gm, aM]
-- let chords' = (transform' (toN i) rotate2) (map toN folia)
-- withMidiSession $ \c -> do
--   mapM_ (playTriad c) (chords' <> [toN i])
--
--
----
----
----
--
--playTriad c t = do
--  print t
--  playNotes c (toNotes t)
--playNotes c ns = do
--  let home = 60
--  forM_ ns $ \n ->
--    send c $ MidiMessage 0 (NoteOn (n + home) 90)
--  threadDelay 1500000
--  forM_ ns $ \n ->
--    send c $ MidiMessage 0 (NoteOff (n + home) 90)
--
--withMidiSession :: (Connection -> IO ()) -> IO ()
--withMidiSession f = do
--  target <- (!! 0) <$> enumerateDestinations
--  c <- openDestination target
--  forM_ [0..100]$ \n ->
--    send c $ MidiMessage 0 (NoteOff n 90)
--  start c
--  f c
--  stop c
