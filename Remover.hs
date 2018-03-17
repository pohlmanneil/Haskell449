module Remover  
( removeIllegalNeighbour  
, removeIllegalMachTask 
, addNeighWeight 
, addAssignWeight  
, removeExpensive
) where

import Myelem
import Sorter

removeIllegalNeighbour :: (Eq a) => [a] -> [[a]] -> [[a]]
removeIllegalNeighbour pair assigns = [ fst x | x <- result, not (snd x)]
  where result = zip assigns (elemTotal pair assigns)
  
removeIllegalMachTask :: (Eq a, Integral a) => [a] -> [[a]] -> [[a]]
removeIllegalMachTask pair assigns = [ fst x | x <- result, not (snd x)]
  where result = zip assigns (elemAtTotal pair assigns)

addNeighWeight :: (Eq a, Integral a) => [a] -> [[a]] -> [[a]]
addNeighWeight trip assigns = [if (snd x) then ((init (fst x)) ++ [(last (fst x))+(trip!!2)]) else (fst x) | x <- result]
  where result = zip assigns (elemTotal trip assigns)

addAssignWeight :: (Eq a, Integral a) => [a] -> [[a]] -> [[a]]
addAssignWeight trip assigns = [if (snd x) then ((init (fst x)) ++ [(last (fst x))+(trip!!2)]) else (fst x) | x <- result]
  where result = zip assigns (elemAtTotal trip assigns)

removeExpensive :: (Eq a, Integral a) => [[a]] -> [a]
removeExpensive assigns = head (sortByPenalty assigns)