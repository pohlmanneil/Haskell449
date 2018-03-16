module PairTripCheck  
( removeIllegalNeighbour  
, removeIllegalMachTask  
, addNeighWeight 
, addAssignWeight  
) where

import Myelem

removeIllegalNeighbour :: (Eq a) => [a] -> [[a]] -> [[a]]
removeIllegalNeighbour pair assigns = [ fst x | x <- result, not (snd x)]
  where result = zip assigns (elemTotal pair assigns)
  
removeIllegalMachTask :: (Eq a, Integral a) => [a] -> [[a]] -> [[a]]
removeIllegalMachTask pair assigns = [ fst x | x <- result, not (snd x)]
  where result = zip assigns (elemAtTotal pair assigns)

removeNonForcedAssign :: (Eq a, Integral a) => [a] -> [[a]] -> [[a]]
removeNonForcedAssign pair assigns = [ fst x | x <- result, snd x]
  where result = zip assigns (elemAtTotal pair assigns)

addNeighWeight :: (Eq a, Integral a) => [a] -> [[a]] -> [[a]]
addNeighWeight trip assigns = [if (snd x) then ((init (fst x)) ++ [(last (fst x))+(trip!!2)]) else (fst x) | x <- result]
  where result = zip assigns (elemTotal trip assigns)

addAssignWeight :: (Eq a, Integral a) => [a] -> [[a]] -> [[a]]
addAssignWeight trip assigns = [if (snd x) then ((init (fst x)) ++ [(last (fst x))+(trip!!2)]) else (fst x) | x <- result]
  where result = zip assigns (elemAtTotal trip assigns)