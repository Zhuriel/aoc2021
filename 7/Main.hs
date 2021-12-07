module Main where
import Data.List.Split

main :: IO ()
main = do
    input <- readFile "input.txt"
    let initialNums = map read $ splitOn "," input
    print . minimum $ fuelCostsA initialNums
    print . minimum $ fuelCostsB initialNums

fuelCostsA :: [Int] -> [Int]
fuelCostsA xs = map (sum . (`diffsTo` xs)) [0..(maximum xs)]

fuelCostsB :: [Int] -> [Int]
fuelCostsB xs = map (sum . map sumOfInts . flip diffsTo xs) [0..(maximum xs)]

sumOfInts :: Int -> Int
sumOfInts n = n * (n + 1) `div` 2

diffsTo :: Int -> [Int] -> [Int]
diffsTo i = map (abs . (i-))
