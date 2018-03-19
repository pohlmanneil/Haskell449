import Data.Char
import Data.List
import System.IO
import Gridparser
import System.FilePath
import Remover
import Sorter
import System.Environment


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

--just brainstorming for what error1 does. file is outputFile and writeTofile may not be correct function

--error1 takes arguments as follows: error1 READFROMHANDLE WRITETOHANDLE WRITETOFILENAME(called by last args) "STRINGTOPRINT"--

error1 :: Handle -> FilePath -> [Char] -> IO b0
error1 inHandle filename str = do
  writeFile filename str
  hClose inHandle
  error str
  
numToLetter :: String -> String
numToLetter "" = ""
numToLetter (x:[])
  | x == '1' = "A"
  | x == '2' = "B"
  | x == '3' = "C"
  | x == '4' = "D"
  | x == '5' = "E"
  | x == '6' = "F"
  | x == '7' = "G"
  | x == '8' = "H"
numToLetter (x:xs)
  | x == '1' = "A" ++" "++ numToLetter xs
  | x == '2' = "B" ++" "++ numToLetter xs
  | x == '3' = "C" ++" "++ numToLetter xs
  | x == '4' = "D" ++" "++ numToLetter xs
  | x == '5' = "E" ++" "++ numToLetter xs
  | x == '6' = "F" ++" "++ numToLetter xs
  | x == '7' = "G" ++" "++ numToLetter xs
  | x == '8' = "H" ++" "++ numToLetter xs

main = do
  args <- getArgs
  --let args = ["test.txt","umm.txt"]
  handle <- openFile (head args) ReadMode --head args
  contents <- hGetContents handle
  let rawIn = lines contents

  
  let y = [trim rawLines | rawLines <- rawIn]
  
  --Index of each header--
  let a = elemIndex "forced partial assignment:" y
  let forceInd = case a of Nothing -> -1
                           Just n -> n
  
  let a = elemIndex "forbidden machine:" y
  let forbidInd = case a of Nothing -> -1
                            Just n -> n
  
  let a = elemIndex "too-near tasks:" y
  let tooTaskInd = case a of Nothing -> -1
                             Just n -> n
  
  let a = elemIndex "machine penalties:" y
  let machineInd = case a of Nothing -> -1
                             Just n -> n
  
  let a = elemIndex "too-near penalities" y
  let tooPenInd = case a of Nothing -> -1
                            Just n -> n
  
  let troubles = if ((forceInd == -1)||(forbidInd == -1)||(tooTaskInd == -1)||(machineInd == -1)||(tooPenInd == -1)) then (error1 handle (last args) "error parsing input") else (readFile (head args))--(readFile (head args)) is placeholder
  
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
  let floatGrid = [(read a :: Float) | a <- grid]
  let intGrid = [floor a | a <- floatGrid, (a == (fromIntegral $ floor a))]
  
  let troubles = if (length intGrid == 64) then (readFile (head args)) else (error1 handle (last args) "machine penalty error") --readFile (head args) is placeholder
  
  
  let assTripGrid = (parseGridToTrip intGrid)
  
  
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
  
  let troubles = if (length assignsNeighPen > 0 ) then (readFile (head args)) else (error1 handle (last args) "No valid solution possible!")
  let assignSorted = sortByPenalty assignsNeighPen

  
  --print (head assignSorted)
  --Write the best assignment to the file--
  let stringAssigns = concat [show a | a <- (head assignSorted)]
  let stringTasks = numToLetter (take 8 stringAssigns)
  writeFile (last args) ("Solution "++stringTasks++"; Quality: "++(drop 8 stringAssigns)) 
  
  hClose handle
  
  