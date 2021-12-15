{-# LANGUAGE TupleSections #-}

module Main where
import Data.List
import Data.Ord
import Data.Ix
import Data.Array.Unboxed
import Data.Char(digitToInt)
import Control.Monad
import Control.Monad.State
import Debug.Trace

main :: IO ()
main = do
    input <- readFile "input.txt"
    let hazardMap = parseHazardMap $ lines input
    --putStrLn . showArray $ hazardMap
    --print $ findPath hazardMap
    --putStrLn . showArray $ enlargeHazardMap hazardMap
    print . findPath . enlargeHazardMap $ hazardMap

findPath :: HazardMap -> Int
findPath hm = (! (snd . bounds $ hm)) . (!! 10) . iterate' (updateMap hm) $ initialPathMap
    where initialPathMap = emptyPathMap hm

type HazardMap = UArray (Int, Int) Int
type Path = Int
type PathMap = UArray (Int, Int) Path

updateMap :: HazardMap -> PathMap -> PathMap
updateMap hm pm = foldl' foldDiag pm [1..((*2) . snd . snd $ bound)]
    where diag x   = filter (validCoord bound) . map (\y -> (x - y, y)) $ [0..x]
          bound    = bounds hm
          foldDiag pm x = foldr (updatePath $! hm) pm (diag x)

updatePath :: HazardMap -> (Int, Int) -> PathMap -> PathMap
updatePath hm pos pm = pm // [(pos, newValue)]
    where curValue  = pm ! pos
          hazValue  = hm ! pos
          adjPos    = filter (validCoord $ bounds pm) $ map (addTuple pos) offsets
          adjacents = map ((+ hazValue) . (pm !)) $ adjPos
          newValue  = minimum (curValue:adjacents)

validCoord :: ((Int, Int), (Int, Int)) -> (Int, Int) -> Bool
validCoord ((xMin, yMin), (xMax, yMax)) (x, y) =
    x >= xMin && x <= xMax && y >= yMin && y <= yMax

diagDistance :: (Int, Int) -> Int
diagDistance (x, y) = x + y

emptyPathMap :: HazardMap -> PathMap
emptyPathMap hm = array bound emptyAssocs // startPoint
    where emptyAssocs = zip (indices hm) (repeat $ emptyPath 10000000000)
          bound       = bounds hm
          startPoint  = [((0, 0), emptyPath 0)]

emptyPath :: Int -> Path
emptyPath i = i

appendPath :: Int -> (Int, Int) -> Path -> Path
appendPath newVal newPos val = newVal + val

offsets :: [(Int, Int)]
offsets = [(0, 1),  (1, 0), (0, -1), (-1, 0)]

addTuple :: (Int, Int) -> (Int, Int) -> (Int, Int)
addTuple (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

mapDuplicate :: Int -> Int -> (Int, Int) -> ((Int, Int), Int) -> ((Int, Int), Int)
mapDuplicate sizeX sizeY (ox, oy) ((x, y), i) = ((x + ox, y + oy), newVal)
    where newVal = remap (i + (ox `div` sizeX) + (oy `div` sizeY))
          remap x | x > 9     = remap (x - 9)
                  | otherwise = x

parseHazardMap :: [String] -> HazardMap
parseHazardMap s = array ((0, 0), (maxX, maxY)) $ join elems
    where elems  = zipWith zip (coords maxX maxY) nums
          maxY   = (length s) - 1
          maxX   = (length $ head s) - 1
          nums   = map (map digitToInt) s

enlargeHazardMap :: HazardMap -> HazardMap
enlargeHazardMap hm = array newBounds newAssocs
    where (boundX, boundY) = snd $ bounds hm
          (sizeX, sizeY)   = (boundX + 1, boundY + 1)
          offsets          = map (\(x,y) -> (x*sizeX, y*sizeY)) . join $ coords 4 4
          newBounds        = ((0, 0), (5 * (sizeX) - 1, 5 * (sizeY) - 1))
          newAssocs        = mapDuplicate sizeX sizeY <$> offsets <*> assocs hm

showArray :: HazardMap -> String
showArray hm = unlines . map (join . map show . map (hm !)) $ coords bX bY
    where (bX, bY) = snd $ bounds hm

coords :: Int -> Int -> [[(Int, Int)]]
coords maxX maxY = map (\y -> map (,y) [0..maxX]) [0..maxY]
