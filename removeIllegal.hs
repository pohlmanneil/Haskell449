import Myelem

removeIllegal :: (Eq a) => [a] -> [[a]] -> [[a]]
removeIllegal pair assigns = [ fst x | x <- result, not (snd x)]
  where result = zip assigns (elemTotal pair assigns)
  
    