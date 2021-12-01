module Main where

main :: IO ()
main = do
    input <- readFile "input.txt"
    print . numIncreases . map read $ lines input
    print . numIncreases . movingSum 3 . map read $ lines input

numIncreases :: [Int] -> Int
numIncreases = length . filter (> 0) . diffAdjacent

diffAdjacent :: [Int] -> [Int]
diffAdjacent (_:[])   = []
diffAdjacent (y:x:xs) = x - y : diffAdjacent (x:xs)

movingSum :: Int -> [Int] -> [Int]
movingSum n (x:xs) = if length subList < n
    then []
    else sum subList : movingSum n xs
    where subList = take n (x:xs)
