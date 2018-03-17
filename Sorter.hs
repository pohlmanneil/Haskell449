module Sorter 
( sortByPenalty 
) where

sortByPenalty :: (Eq a, Integral a) => [[a]] -> [[a]]
sortByPenalty [] = []
sortByPenalty (as:assigns) =
  let smallerSorted = sortByPenalty [a | a <- assigns, (last a) <= (last as)]
      biggerSorted = sortByPenalty [a | a <- assigns, (last a) > (last as)]
  in  smallerSorted ++ [as] ++ biggerSorted