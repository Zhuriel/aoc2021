{-# LANGUAGE TupleSections #-}

module Main where
import Control.Monad
import Control.Applicative
import Data.Char(digitToInt)
import Data.List

main :: IO ()
main = do
    input <- readFile "input.txt"
    let heightMap = map (map digitToInt) . lines $ input
    let minima = globalMinima heightMap
    print $ partA heightMap
    print $ partB heightMap

partA :: HeightMap -> Int
partA hm = sum mv + length mv
    where mv = minimumValues hm

partB :: HeightMap -> Int
partB hm = case basinSizes hm of
    Just l  -> product . take 3 . reverse . sort . map length $ l
    Nothing -> error "list should not be empty"

basinSizes :: HeightMap -> Maybe Basins
basinSizes hm = fmap fst . find (null . snd) $ iterate growBasins initial
    where initial         = (map (:[]) globalMinCoords, basinCoords)
          globalMinCoords = getCoords . globalMinima $ hm
          basinCoords     = getCoords . getBasins $ hm

type HeightMap = [[Int]]
type Basins = [Coords]
type Coords = [(Int, Int)]

minimumValues :: HeightMap -> [Int]
minimumValues hm = map fst . filter snd $ zipped
    where zipped = zip (join hm) (join . globalMinima $ hm)

globalMinima :: HeightMap -> [[Bool]]
globalMinima hm = zipWith (zipWith (&&)) rows cols
    where rows    = tail $ rowMinima preProc
          cols    = map tail $ colMinima preProc
          preProc = replicate ((+1) . length . head $ hm) 10 : map (10:) hm

getCoords :: [[Bool]] -> Coords
getCoords bs = map fst . filter snd . join $ zipped
    where zipped = zipWith zip coords bs
          coords = map (\y -> map (,y) [0..]) [0..]

colMinima :: HeightMap -> [[Bool]]
colMinima []            = []
colMinima [x1, x2]      = [zipWith (<) x2 x1]
colMinima (x1:x2:x3:xs) = zipWith (&&) above below : colMinima (x2:x3:xs)
    where above = zipWith (<) x2 x1
          below = zipWith (<) x2 x3

rowMinima :: HeightMap -> [[Bool]]
rowMinima = map rowMinimum

rowMinimum :: [Int] -> [Bool]
rowMinimum []            = []
rowMinimum [x1, x2]      = [x2 < x1]
rowMinimum (x1:x2:x3:xs) = (x2 < x1 && x2 < x3) : rowMinimum (x2:x3:xs)

getBasins :: HeightMap -> [[Bool]]
getBasins = map (map (<9))

getAdjacents :: (Int, Int) -> Coords
getAdjacents (x, y) = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]

growBasins :: (Basins, Coords) -> (Basins, Coords)
growBasins ([], xs)   = ([], xs)
growBasins (b:bs, xs) = (nb:rb, rx)
    where (nb, nx) = growBasin b xs
          (rb, rx) = growBasins (bs, nx)

growBasin :: Coords -> Coords -> (Coords, Coords)
growBasin b []                  = (b, [])
growBasin b (x:xs) | x `elem` b = (rb, rx)
                   | adjacent   = (x:rb, rx)
                   | otherwise  = (rb, x:rx)
    where adjacent = any (`elem` b) $ getAdjacents x
          (rb, rx) = growBasin b xs
