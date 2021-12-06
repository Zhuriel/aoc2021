module Main where
import Data.List.Split
import Data.List
import qualified Data.HashMap.Strict as HashMap
import Control.Monad
import Debug.Trace

main :: IO ()
main = do
    input <- readFile "input.txt"
    print . partA $ lines input
    print . partB $ lines input

partA :: [String] -> Int
partA input = length $ HashMap.keys finalMap
    where finalMap = checkMultiple segs
          segs     = filter isOrthogonal . map parseSeg $ input

partB :: [String] -> Int
partB input = length $ HashMap.keys finalMap
    where finalMap = checkMultiple segs
          segs     = map parseSeg input


checkMultiple :: [Seg] -> PointMap
checkMultiple = HashMap.filter (>= 2) . mapSegs

mapSegs :: [Seg] -> PointMap
mapSegs = foldr addPoint HashMap.empty . join . map genSeg

parseTest :: [String] -> [Seg]
parseTest = map parseSeg

type Point = (Int, Int)
type Seg = (Point, Point)
type PointMap = HashMap.HashMap Point Int

parsePoint :: String -> Point
parsePoint spec = (x, y)
    where (x:y:_) = map read $ splitOn "," spec

parseSeg :: String -> Seg
parseSeg spec = ((xStart, yStart), (xEnd, yEnd))
    where (a:_:b:_)        = words spec
          (pointA, pointB) = (parsePoint a, parsePoint b)
          (xStart, yStart) = pointA
          (xEnd, yEnd)     = pointB

segDir :: Seg -> (Int, Int)
segDir ((ax, ay), (bx, by)) = (signum (bx - ax), signum (by - ay))

genSeg :: Seg -> [Point]
genSeg s = [(ax + d * dirX, ay + d * dirY) | d <- [0..len]]
    where ((ax, ay), (bx, by)) = s
          len          = max (abs (bx - ax)) (abs (by - ay))
          (dirX, dirY) = segDir s

addPoint :: Point -> PointMap -> PointMap
addPoint = HashMap.alter incrementPointVal

incrementPointVal :: Maybe Int -> Maybe Int
incrementPointVal Nothing = Just 1
incrementPointVal (Just x) = Just (x + 1)

isOrthogonal :: Seg -> Bool
isOrthogonal ((ax, ay), (bx, by)) = ax == bx || ay == by
