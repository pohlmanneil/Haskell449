import Data.Char
import Data.List
import System.IO
import Gridparser
import System.FilePath
import Remover
import Sorter


trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace
   
parseStringPairs :: (Integral b) => [Char] -> [b]
parseStringPairs "" = []
parseStringPairs rawStuff = (myCharToInt (rawStuff!!1)):[myCharToInt (rawStuff!!3)]

parseStringTrips :: [Char] -> [Integer]
parseStringTrips "" = []
parseStringTrips rawStuff = (myCharToInt (rawStuff!!1)):(myCharToInt (rawStuff!!3)):(myStrToInt (init (drop 5 rawStuff)))--[myCharToInt (rawStuff!!5)]
  
myCharToInt :: (Integral b) => Char -> b
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

myStrToInt :: [Char] -> [Integer]
myStrToInt "" = []
myStrToInt rawStr = [read rawStr :: Integer]

main = do
  --args <- getArgs
  --inputFile <- (head args)
  --outputFile <- (last args)
  handle <- openFile "test.txt" ReadMode
  contents <- hGetContents handle
  let rawIn = lines contents
  
  let y = [trim rawLines | rawLines <- rawIn]
  
  --Index of each header--
  let Just forceInd = elemIndex "forced partial assignment:" y
  
  let Just forbidInd = elemIndex "forbidden machine:" y
  
  let Just tooTaskInd = elemIndex "too-near tasks:" y
  
  let Just machineInd = elemIndex "machine penalties:" y

  let Just tooPenInd = elemIndex "too-near penalities" y
  
  
  --raw (Strings) data--
  let rawForced = take (forbidInd-(forceInd+1)) (drop (forceInd+1) y)
  
  let rawForbid = take (tooTaskInd-(forbidInd+1)) (drop (forbidInd+1) y)
  
  let rawIllegPair = take (machineInd-(tooTaskInd+1)) (drop (tooTaskInd+1) y)
  
  let rawGrid = take (tooPenInd-(machineInd+1)) (drop (machineInd+1) y)
  
  let rawPenalTrip = drop (tooPenInd+1) y
  
  
  --turning data into usable Ints--
  let forcedInt = [parseStringPairs forcedString | forcedString <- rawForced, (length forcedString) > 0]
  
  let forbidInt = [parseStringPairs forbidString | forbidString <- rawForbid, (length forbidString) > 0]
  
  let illegPairInt = [parseStringPairs illegString | illegString <- rawIllegPair, (length illegString) > 0]
  
  let grid = concat [ words gridEl | gridEl <- rawGrid]
  let intGrid = [(read a :: Integer) | a <- grid]
  let assTripGrid = parseGridToTrip intGrid
  
  
  let penalTripInt = [parseStringTrips penalString | penalString <- rawPenalTrip, (length penalString) > 0]
  
  
  
  --creating all assignments with 9th element as penalty
  let allAssigns = permutations [1..8]
  let assignsWithWeight = [a ++ [0] | a <- allAssigns]
  
  
  --remove illegal assignments, then add weights--
  let assignsNoMachTask = removeMulMachTask forbidInt assignsWithWeight
  let assignsNoIllNeigh = removeMulIllNeigh illegPairInt assignsNoMachTask
  let assignsNoForce = removeMulNonForced forcedInt assignsNoIllNeigh
  let assignsAssPen = addMulAss assTripGrid assignsNoForce
  let assignsNeighPen = addMulNeigh penalTripInt assignsAssPen
  
  
  --sort the assignments--
  let assignSorted = sortByPenalty assignsNeighPen

  
  print (head assignSorted)
  
  
  hClose handle
  
  --handle <- openFile "test2.txt" WriteMode
  
  