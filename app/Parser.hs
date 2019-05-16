module Parser
    ( parseAf
    , parseOp
    , parseCmd
    , parseHead
    ) where

import Util
import Text.ParserCombinators.ReadP
import Data.Char
import Language.Dung.AF as AF

adjacencyToAfAtts :: [[Int]] -> [(Int, Int)]
adjacencyToAfAtts = concat . (map (\(i,l) -> zip (repeat i) l)) . (zip [0..])

adjacencyToAf :: [[Int]] -> AF.DungAF Int
adjacencyToAf m =
    let atts = adjacencyToAfAtts m
    in AF.AF (snub $ concat [ [x,y] | (x,y) <- atts ]) atts

parseAdjacency :: ReadP [[String]]
parseAdjacency = sepBy
    (skipSpaces >> sepBy
        (many1 $ satisfy isDigit)
        (satisfy isSpace)
    )
    (skipSpaces >> char ';')

parseAf :: ReadP (String, AF.DungAF Int)
parseAf = do
    name <- parseAssignement
    adjacencyMatrix <- parseAdjacency
    return (name, adjacencyToAf $ map toInts adjacencyMatrix)
    where toInts = map (read :: String -> Int)

parseHead :: ReadP String
parseHead = do
    skipSpaces
    head <- many1 $ satisfy isAlpha
    (skipMany1 (satisfy isSpace)) <++ eof
    return head

parseCmd :: ReadP String
parseCmd = do
    skipSpaces
    char ':'
    parseHead

parseAssignement :: ReadP String
parseAssignement = do
    name <- parseHead
    skipSpaces
    string ":="
    skipSpaces
    return name

parseOp :: ReadP (String, String, String)
parseOp = do
    resultName <- parseAssignement
    name1 <- parseHead
    name2 <- parseHead
    return (resultName, name1, name2)
