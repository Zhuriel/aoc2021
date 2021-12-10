module Main where
import Data.Either
import Data.List

main :: IO ()
main = do
    input <- readFile "input.txt"
    print . partA . lines $ input
    print . partB . lines $ input

partA :: [String] -> Int
partA = sum . map bracketValue . rights . map (parse "")

partB :: [String] -> Int
partB = middleElement . sort . map (foldl partBScore 0) . lefts . map (parse "")

type ParseResult = Either String Char

middleElement :: [a] -> a
middleElement l = l !! (`div` 2)(length l)

partBScore :: Int -> Char -> Int
partBScore a c = a * 5 + bracketValue c

bracketValue :: Char -> Int
bracketValue ')' = 3
bracketValue ']' = 57
bracketValue '}' = 1197
bracketValue '>' = 25137
bracketValue '(' = 1
bracketValue '[' = 2
bracketValue '{' = 3
bracketValue '<' = 4
bracketValue _   = 0

matchingBracket :: Char -> Char -> Bool
matchingBracket '(' ')' = True
matchingBracket '[' ']' = True
matchingBracket '{' '}' = True
matchingBracket '<' '>' = True
matchingBracket _ _     = False

isOpening :: Char -> Bool
isOpening = (`elem` "([{<")

parse :: String -> String -> ParseResult
parse stack     []                            = Left stack
parse stack     (b:str) | isOpening b         = parse (b:stack) str
parse (a:stack) (b:str) | matchingBracket a b = parse stack str
                        | otherwise           = Right b
