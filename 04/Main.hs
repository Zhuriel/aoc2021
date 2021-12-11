module Main where
import Data.List.Split
import Data.List
import Data.Ord
import Control.Monad

main :: IO ()
main = do
    input <- readFile "input.txt"
    let inLines = lines input
    let numbers = map read . splitOn "," $ head inLines
    let boards = parseBoards $ tail inLines
    print $ partA boards numbers
    print $ partB boards numbers

partA :: [BingoBoard] -> [Int] -> Int
partA boards nums = last . shortestList $ map (playBingo nums) boards

partB :: [BingoBoard] -> [Int] -> Int
partB boards nums = last . longestList $ map (playBingo nums) boards

shortestList :: [[a]] -> [a]
shortestList = minimumBy (comparing length)

longestList :: [[a]] -> [a]
longestList = maximumBy (comparing length)

data BingoNumber = Unmarked Int | Marked Int
type BingoBoard = [[BingoNumber]]

playBingo :: [Int] -> BingoBoard -> [Int]
playBingo nums board = zipWith (*) nums . map sumBoard $ game
    where states = scanl markBoard board nums
          game   = takeUntilIncl checkBoard . drop 1 $ states

markNum :: Int -> BingoNumber -> BingoNumber
markNum _ (Marked x)               = Marked x
markNum y (Unmarked x) | x == y    = Marked x
                       | otherwise = Unmarked x

parseNum :: String -> BingoNumber
parseNum s = Unmarked (read s)

checkNum :: BingoNumber -> Bool
checkNum (Marked _) = True
checkNum (Unmarked _) = False

numValue :: BingoNumber -> Int
numValue (Marked x) = 0
numValue (Unmarked x) = x

parseBoard :: [String] -> BingoBoard
parseBoard = (map . map) parseNum . map words

parseBoards :: [String] -> [BingoBoard]
parseBoards [] = []
parseBoards lst = parseBoard boardString : parseBoards stringTail
    where (boardString, stringTail) = splitAt 5 $ drop 1 lst

markBoard :: BingoBoard -> Int -> BingoBoard
markBoard board n = (map . map) (markNum n) board

checkBoard :: BingoBoard -> Bool
checkBoard board = or rows || or cols
    where rows = map and boolBoard
          cols = foldr1 (zipWith (&&)) boolBoard
          boolBoard = (map . map) checkNum board

sumBoard :: BingoBoard -> Int
sumBoard = sum . map numValue . join

takeUntilIncl :: (a -> Bool) -> [a] -> [a]
takeUntilIncl _ [] = []
takeUntilIncl f (x:xs) | f x       = [x]
                       | otherwise = x : takeUntilIncl f xs
