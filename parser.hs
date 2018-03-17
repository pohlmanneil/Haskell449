import Text.ParserCombinators.Parsec
import System.IO
import Control.Monad
import System.Environment
import Data.List

data Text = TextChunk String deriving (Show)

fileParser :: GenParser Char st [Text]
fileParser = do
    string "Name:"
    nameValue <- many (noneOf "forced partial assignment:")
    string "forced partial assignment:"
    forcedPartialAssignment <- many (noneOf "forbidden machine:")
    string "forbidden machine:"
    forbiddenMachine <- many (noneOf "too-near tasks:")
    string "too-near tasks:"
    machinePenalties <- many (noneOf "machine penalties:")
    string "machine penalties:"
    tooNearPenalities <- many (noneOf "too-near penalities")
    string "too-near penalities"
    return $ [TextChunk forcedPartialAssignment, TextChunk forbiddenMachine, TextChunk machinePenalties, TextChunk forbiddenMachine]

parseS :: String -> Either ParseError [Text]
parseS input = parse fileParser "" input

compareMe c f 0 = return()
compareMe c f s = do
    let p = c !! s
    let y = stripPrefix (p) (f)
    putStrLn (p)
    if y == (Just "") then putStrLn ("match found")
    else compareMe (c) (f) (s-1)

main = do
    args <- getArgs
    contents <- readFile (head args)
    let dataString = "Name: minimalisticexampleforced partial assignment:forbidden machine:too-near tasks:machine penalties: 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1too-near penalities "
    print dataString

    parseS dataString
    --writeFile (last args) (contents)