module Myelem 
( elemTotal 
, elem2 
) where 

import Data.List

elemTotal :: (Eq a) => [a] -> [[a]] -> [Bool]
elemTotal pair assigns = map (elem2 pair) assigns

elem2 :: (Eq a) => [a] -> [a] -> Bool
elem2 pair assign = (elem x0 assign) && (elem x1 assign) && ((ind x1 assign) - (ind x0 assign)==1)
  where x0 = pair!!0
        x1 = pair!!1
        ind x y = case elemIndex x y of
          Just n -> n