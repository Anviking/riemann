module Negative where

import           Notes

invertAround :: Note -> Note -> Note
invertAround center x =
  let delta = x - center
  in center - delta

invertTriad :: Note -> Triad -> Maybe Triad
invertTriad center = classify . map (invertAround center) . toNotes
