{-# LANGUAGE TupleSections #-}

module Main where
import Data.Array.Unboxed
import Data.Ix
import Data.Tuple
import Control.Monad
import Debug.Trace

main :: IO ()
main = do
    input <- readFile "input.txt"
    let (spec:_, _:img) = break null $ lines input
    let parsedImg = parseImg img
    print $ partA spec parsedImg
    print $ partB spec parsedImg

partA :: String -> Image -> Int
partA spec = length . filter (=='#') . elems . fst . iterateImg spec 2

partB :: String -> Image -> Int
partB spec = length . filter (=='#') . elems . fst . iterateImg spec 50

iterateImg :: String -> Int -> Image -> Image
iterateImg spec n = (!! n) . iterate (updateImg spec)

type Image = (UArray (Int, Int) Char, Char)

updateImg :: String -> Image -> Image
updateImg spec i@(img, fill) = remBorder 1 (fst expanded // newAssocs, newFill)
    where expanded  = addBorder 2 i
          points    = join . coords . modifyBounds 1 $ bounds img
          newAssocs = map ((,) <$> id <*> pixVal spec expanded) points
          newFill   = if fill == '#' then last spec else head spec

pixVal :: String -> Image -> (Int, Int) -> Char
pixVal spec (img, fill) (x, y) = spec !! value
    where positions      = map swap $ (,) <$> [y-1..y+1] <*> [x-1..x+1]
          values         = map (img !) positions
          value          = foldl makeNumber 0 values
          makeNumber n c = 2 * n + if c == '#' then 1 else 0

remBorder :: Int -> Image -> Image
remBorder n (img, fill) = (array newBounds newAssocs, fill)
    where newBounds = modifyBounds (-n) $ bounds img
          newAssocs = filter (inBounds newBounds . fst) $ assocs img

inBounds :: ((Int, Int), (Int, Int)) -> (Int, Int) -> Bool
inBounds ((xMin, yMin), (xMax, yMax)) (x, y) =
    x >= xMin && x <= xMax && y >= yMin && y <= yMax

addBorder :: Int -> Image -> Image
addBorder n (img, fill) = (emptyArray // assocs img, fill)
    where newBounds  = modifyBounds n $ bounds img
          emptyArray = array newBounds . map (,fill) . join $ coords newBounds

modifyBounds :: Int -> ((Int, Int), (Int, Int)) -> ((Int, Int), (Int, Int))
modifyBounds n ((x1, y1), (x2, y2)) = ((x1 - n, y1 - n), (x2 + n, y2 + n))

parseImg :: [String] -> Image
parseImg s = (arr, '.')
    where arr    = array bounds $ join (zipWith zip (coords bounds) s)
          maxX   = (length . head $ s) - 1
          maxY   = length s - 1
          bounds = ((0, 0), (maxX, maxY))

coords :: ((Int, Int), (Int, Int)) -> [[(Int, Int)]]
coords ((xMin, yMin), (xMax, yMax)) =
    map (\y -> map (,y) [xMin..xMax]) [yMin..yMax]
