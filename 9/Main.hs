module Main where
import Control.Monad
import Control.Applicative
import Data.Char(digitToInt)

main :: IO ()
main = do
    input <- readFile "test.txt"
    let heightMap = map (map digitToInt) . lines $ input
    let minima = globalMinima heightMap
    print $ partA heightMap
    print $ getBasins heightMap

partA :: HeightMap -> Int
partA hm = sum mv + length mv
    where mv = minimumValues hm

type HeightMap = [[Int]]
data BasinMapStatus = NotBasin | Basin | Edge | Reduced | Complete
    deriving (Show)
type BasinMapTile = ((BasinMapStatus, BasinMapStatus), Int)
type BasinMap = [[BasinMapTile]]

minimumValues :: HeightMap -> [Int]
minimumValues hm = map fst . filter snd $ zipped
    where zipped = zip (join hm) (join . globalMinima $ hm)

globalMinima :: HeightMap -> [[Bool]]
globalMinima hm = zipWith (zipWith (&&)) rows cols
    where rows    = tail $ rowMinima preProc
          cols    = map tail $ colMinima preProc
          preProc = replicate ((+1) . length . head $ hm) 10 : map (10:) hm

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

expandBasins :: HeightMap -> [[Bool]] -> [[Bool]]
expandBasins hm bm | noChange  = bm
                   | otherwise = expandBasins hm newBm
    where newBm    = expandBasinsStep hm bm
          noChange = and . map and . zipWith (zipWith (==)) bm $ newBm

expandBasinsStep :: HeightMap -> [[Bool]] -> [[Bool]]
expandBasinsStep hm bm = map (map snd) hStep
    where hStep  = map expandPrim . map ((10, False):) $ transpose vStep
          vStep  = map expandPrim . map ((10, False):) $ transpose zipped
          zipped = zipWith zip hm bm

expandPrim :: [(Int, Bool)] -> [(Int, Bool)]
expandPrim [] = []
expandPrim [x] = error "should not encounter the case of a single element"
expandPrim [(_, True), (x, _)]      = [(x, x < 9)]
expandPrim [(_, False), x]          = [x]
expandPrim ((_, True):(x, xb):x2:xs) = (x, x < 9) : expandPrim ((x,xb):x2:xs)
expandPrim (_:(x, xb):(x2, True):xs) = (x, x < 9) : expandPrim ((x,xb):(x2, True):xs)
expandPrim (_:x:x2:xs)              = x : expandPrim (x:x2:xs)

-- thanks random blog
transpose :: [[a]] -> [[a]]
transpose = getZipList . traverse ZipList
