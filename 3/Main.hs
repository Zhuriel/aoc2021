module Main where
import Data.Char(digitToInt)
import Debug.Trace

main :: IO ()
main = do
    input <- readFile "input.txt"
    print . partA $ lines input
    print . partB $ lines input

partA :: [String] -> Int
partA input = gamma input * epsilon input
    where
        gamma = listBoolToInt . majority . map toListBits
        epsilon = listBoolToInt . map not . majority . map toListBits

partB :: [String] -> Int
partB input = oxy input * co2 input
    where
        oxy = listBoolToInt . map (== 1) . filterByBits True 0 . map toListBits
        co2 = listBoolToInt . map (== 1) . filterByBits False 0 . map toListBits

filterByBits :: Bool -> Int -> [[Int]] -> [Int]
filterByBits _ _ [] = []
filterByBits _ _ (x:[]) = x
filterByBits b n lst = filterByBits b (n + 1) reducedList
    where
        reducedList = filter ((== fromEnum filterVal) . head . drop n) lst
        filterVal = if b then majorityVal else not majorityVal
        majorityVal = head . drop n . majority $ lst

listBoolToInt :: [Bool] -> Int
listBoolToInt = foldl (\s b -> 2 * s + fromEnum b) 0

majority :: [[Int]] -> [Bool]
majority lst = map (cmp) $ countBits lst
    where
        half = length lst `div` 2
        cmp = if even $ length lst then (>= half) else (> half)

countBits :: [[Int]] -> [Int]
countBits (x:xs) = foldr (zipWith (+)) x xs

toListBits :: [Char] -> [Int]
toListBits = map digitToInt
