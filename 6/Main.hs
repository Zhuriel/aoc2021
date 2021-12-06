module Main where
import qualified Data.Vector as V
import Data.List.Split

main :: IO ()
main = do
    input <- readFile "input.txt"
    let initialNums = map read $ splitOn "," input
    print $ partA initialNums
    print $ partB initialNums

partA :: [Int] -> Int
partA = simFish 80

partB :: [Int] -> Int
partB = simFish 256

simFish :: Int -> [Int] -> Int
simFish n = V.sum . head . drop n . iterate iterateFish . initializeVector

initializeVector :: [Int] -> V.Vector Int
initializeVector nums = V.fromList $ map (flip countOccurrences nums) [0..8]

countOccurrences :: Int -> [Int] -> Int
countOccurrences x = length . filter (==x)

iterateFish :: V.Vector Int -> V.Vector Int
iterateFish v = flip (V.//) [(6, at7 + numNew)] . flip V.snoc numNew $ V.tail v
    where at7    = v V.! 7
          numNew = V.head v
