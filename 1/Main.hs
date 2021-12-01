module Main where

main :: IO ()
main = do
    input <- readFile "input.txt"
    print . numIncreases . map read $ lines input
    print . numIncreases . movingSum3 . map read $ lines input

numIncreases = length . filter (> 0) . diffAdjacent

diffAdjacent :: [Int] -> [Int]
diffAdjacent (_:[]) = []
diffAdjacent (y:x:xs) = x - y : (diffAdjacent (x:xs))

movingSum3 :: [Int] -> [Int]
movingSum3 (_:_:[]) = []
movingSum3 (a:b:c:xs) = a + b + c : (movingSum3 (b:c:xs))
