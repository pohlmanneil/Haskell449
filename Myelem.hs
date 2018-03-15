module Myelem 
( elemTotal 
, elem2 
, elemAt  
, elemAtTotal 
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
          

elemAtTotal :: (Eq a, Integral a) => [a] -> [[a]] -> [Bool]
elemAtTotal pair assigns = map (elemAt pair) assigns

elemAt :: (Eq a, Integral a) => [a] -> [a] -> Bool
elemAt pair assign = ((ind task assign) == mach)
  where task = (pair!!1)
        mach = case (pair!!0) of
          1 -> 0
          2 -> 1
          3 -> 2
          4 -> 3
          5 -> 4
          6 -> 5
          7 -> 6
          8 -> 7
        ind x y = case elemIndex x y of
          Just n -> n