import Myelem

removeIllegalNeighbour :: (Eq a) => [a] -> [[a]] -> [[a]]
removeIllegalNeighbour pair assigns = [ fst x | x <- result, not (snd x)]
  where result = zip assigns (elemTotal pair assigns)
  
removeIllegalMachTask :: (Eq a, Integral a) => [a] -> [[a]] -> [[a]]
removeIllegalMachTask pair assigns = [ fst x | x <- result, not (snd x)]
  where result = zip assigns (elemAtTotal pair assigns)
    