module Gridparser
(parseGridToTrip 
) where

import Data.List

parseGridToTrip :: [Integer] -> [[Integer]]
parseGridToTrip gridThing = 
  let pairs = [[x,y] | x <- [1..8], y <- [1..8]]
  in [(fst w) ++ [(snd w)] | w <- (zip pairs gridThing)]