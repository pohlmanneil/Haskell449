module Remover  
( removeIllegalNeighbour  
, removeIllegalMachTask  
, removeNonForcedAssign  
, addNeighWeight 
, addAssignWeight  
, removeExpensive  
, removeMulMachTask
, removeMulIllNeigh  
, removeMulNonForced  
, addMulNeigh
, addMulAss
) where

import Myelem
import Sorter

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
  
removeExpensive :: (Eq a, Integral a) => [[a]] -> [a]
removeExpensive assigns = head (sortByPenalty assigns)

--remove ALL the illegal neighbours--
removeMulIllNeigh [] assigns = assigns
removeMulIllNeigh _ [] = []
removeMulIllNeigh pairs assigns = removeMulIllNeigh (tail pairs) (removeIllegalNeighbour (head pairs) assigns)

--remove ALL the illegal mach task
removeMulMachTask [] assigns = assigns
removeMulMachTask _ [] = []
removeMulMachTask pairs assigns = removeMulMachTask (tail pairs) (removeIllegalMachTask (head pairs) assigns)

--remove ALL the non forced assignments--
removeMulNonForced [] assigns = assigns
removeMulNonForced _ [] = []
removeMulNonForced pairs assigns = removeMulNonForced (tail pairs) (removeNonForcedAssign (head pairs) assigns)

--add ALL the neighbour penalties--
addMulNeigh :: [[Integer]] -> [[Integer]] -> [[Integer]]
addMulNeigh [] assigns = assigns
addMulNeigh trips assigns = addMulNeigh (tail trips) (addNeighWeight (head trips) assigns)

--add ALL the assignment penalties--
addMulAss :: [[Integer]] -> [[Integer]] -> [[Integer]]
addMulAss [] assigns = assigns
addMulAss trips assigns = addMulAss (tail trips) (addAssignWeight (head trips) assigns)



