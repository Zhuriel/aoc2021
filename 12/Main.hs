module Main where
import Data.List.Split
import Control.Monad
import qualified Data.Map as M
import Data.Hashable
import Data.Tuple
import Data.Char
import Data.List

main :: IO ()
main = do
    input <- readFile "input.txt"
    let caveList = map parseCave $ lines input
    let caveMap = parseCaveList (lines input) M.empty
    let paths = getPaths caveMap
    -- putStrLn . join . intersperse "\n" . map show $ paths
    print . partA $ caveMap
    print . partB $ caveMap

partA :: CaveMap -> Int
partA = length . filter (not . hasRepeatingSmall) . getPaths

partB :: CaveMap -> Int
partB = length . getPaths

data Cave = Large String | Small String | Repeated String
    deriving (Show, Eq)
instance Hashable Cave where
    hashWithSalt salt (Large c)    = hashWithSalt salt c
    hashWithSalt salt (Small c)    = hashWithSalt salt c
    hashWithSalt salt (Repeated c) = hashWithSalt salt ('_':c)
instance Ord Cave where
    compare (Large s1) (Large s2) = compare s1 s2
    compare (Large _)  (Small _)  = GT
    compare (Small _)  (Large _)  = LT
    compare (Small s1) (Small s2) = compare s1 s2
    compare (Large _)    (Repeated _)   = GT
    compare (Small _)    (Repeated _)   = GT
    compare (Repeated _) (Large _)      = LT
    compare (Repeated _) (Small _)      = LT
    compare (Repeated s1) (Repeated s2) = compare s1 s2

type CavePath = [Cave]
type CaveMap = M.Map Cave [Cave]

startPoint :: Cave
startPoint = Small "start"

getPaths :: CaveMap -> [CavePath]
getPaths m = filter validPath paths
    where paths = findDone $ iterate (>>= continuePath m) [[startPoint]]

findDone :: [[[a]]] -> [[a]]
findDone (x:y:xs) | (length $ join x) == (length $ join y) = x
                  | otherwise                              = findDone (y:xs)

validPath :: CavePath -> Bool
validPath (endPoint:_) = True
validPath _                 = False

continuePath :: CaveMap -> CavePath -> [CavePath]
continuePath _ p@((Small "end"):_) = [p]
continuePath m (c:cs)              = map (:c:cs) $ continuations m (c:cs)

continuations :: CaveMap -> CavePath -> [Cave]
continuations m ((Repeated s):cs) = filterCand (c:cs) $ m M.! c
    where c = Small s

continuations m p@(c:cs) | hasRepeatingSmall p = filterCand p nc
                         | otherwise = join . map makeRepeated $ nc
    where nc = m M.! c
          makeRepeated (Small "start")              = []
          makeRepeated new@(Small x) | new `elem` p = [Repeated x]
          makeRepeated new                          = [new]

filterCand :: CavePath -> [Cave] -> [Cave]
filterCand p = filter f
    where f (Small "start")           = False
          f c@(Small _) | c `elem` p  = False
          f _                         = True


hasRepeatingSmall :: CavePath -> Bool
hasRepeatingSmall = not . null . filter f
    where f (Repeated _) = True
          f _            = False

parseCaveList :: [String] -> CaveMap -> CaveMap
parseCaveList []     m = m
parseCaveList (s:ss) m = parseCaveList ss newMap
    where newMap   = addCave fstAdded (swap caves)
          fstAdded = addCave m caves
          caves    = parseCave s

addCave :: CaveMap -> (Cave, Cave) -> CaveMap
addCave m (c1, c2) = M.alter (appendCave c2) c1 m
    where appendCave c Nothing   = Just [c]
          appendCave c (Just cs) = Just (c:cs)

parseCave :: String -> (Cave, Cave)
parseCave s = (getCave a, getCave b)
    where (a:b:_) = splitOn "-" s
          getCave c | any isUpper c = Large c
                    | otherwise     = Small c
