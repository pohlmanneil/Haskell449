import Data.Char
import Data.List
import System.IO
import Gridparser
import System.FilePath


trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace
   
--parseStringPairs :: (String a, Integral b, Eq a) => a -> [b]
parseStringPairs "" = []
parseStringPairs rawStuff = (myCharToInt (rawStuff!!1)):[myCharToInt (rawStuff!!3)]
  
--myCharToInt :: (Char a, Integral b) => a -> b
myCharToInt rawStr
    | rawStr == 'A' = 1
	| rawStr == 'B' = 2
	| rawStr == 'C' = 3
    | rawStr == 'D' = 4
	| rawStr == 'E' = 5
	| rawStr == 'F' = 6
	| rawStr == 'G' = 7
	| rawStr == 'H' = 8
	| rawStr == '1' = 1
	| rawStr == '2' = 2
	| rawStr == '3' = 3
	| rawStr == '4' = 4
	| rawStr == '5' = 5
	| rawStr == '6' = 6
	| rawStr == '7' = 7
	| rawStr == '8' = 8


main = do
  --args <- getArgs
  --inputFile <- (head args)
  --outputFile <- (last args)
  handle <- openFile "test.txt" ReadMode
  contents <- hGetContents handle
  let rawIn = lines contents
  
  let y = [trim rawLines | rawLines <- rawIn]
  
  let Just forceInd = elemIndex "forced partial assignment:" y
  
  let Just forbidInd = elemIndex "forbidden machine:" y
  
  let Just tooTaskInd = elemIndex "too-near tasks:" y
  
  let Just machineInd = elemIndex "machine penalties:" y

  let Just tooPenInd = elemIndex "too-near penalities" y
  
  
  
  let rawForced = take (forbidInd-(forceInd+1)) (drop (forceInd+1) y)
  
  let rawForbid = take (tooTaskInd-(forbidInd+1)) (drop (forbidInd+1) y)
  
  let rawIllegPair = take (machineInd-(tooTaskInd+1)) (drop (tooTaskInd+1) y)
  
  let rawGrid = take (tooPenInd-(machineInd+1)) (drop (machineInd+1) y)
  let grid = concat [ words gridEl | gridEl <- rawGrid]
  let intGrid = [(read a :: Int) | a<-grid]
  
  let rawPenalTrip = drop (tooPenInd+1) y
  
  let forcedInt = [parseStringPairs forceString | forceString <- rawForced, (length forceString) > 0]
  
  print forcedInt
  print rawForced
--  print rawForbid
 -- print rawIllegPair
  --print rawPenalTrip
  
  
  
  
  --print intGrid
  
  
 
  
  --putStr contents
  hClose handle
  
  --handle <- openFile "test2.txt" WriteMode
  
  
  --0-7 + 8*n