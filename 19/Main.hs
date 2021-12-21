module Main where
import Data.List
import Data.List.Split
import Data.Ord
import Debug.Trace

main :: IO ()
main = do
    input <- readFile "input.txt"
    let scannerData = parse . lines $ input
    print $ partB scannerData
    print $ partA scannerData

partA :: [ScannerData] -> Int
partA (s:ss) = length . fst $ matchAll [s] ss

partB :: [ScannerData] -> Int
partB (s:ss) = maximum $ map (\x -> maximum . map (manhattan x) $ spos) spos
    where (_, spos) = matchAll [s] ss

type Position = (Int, Int, Int)
type ScannerData = [Position]
type RelativeData = (ScannerData, Position)
type MatchData = (ScannerData, Position, Position, Int)

matchAll :: [ScannerData] -> [ScannerData] -> (ScannerData, [Position])
matchAll ref []        = (foldr1 union ref, [])
matchAll ref remaining = dbg (rd, spos)
    where matches      = map (`getMatches` remaining) ref
          newMatches   = foldr1 union . map (\(x,_,_) -> x) $ matches
          newRemaining = foldr1 intersect . map (\(_,y,_) -> y) $ matches
          newSpos      = foldr1 union . map (\(_,_,z) -> z) $ matches
          (rd, sp)     = matchAll (newMatches `union` ref) newRemaining
          spos         = newSpos `union` sp
          dbg          = trace (show . length $ newMatches)

getMatches :: ScannerData -> [ScannerData] -> ([ScannerData], [ScannerData], [Position])
getMatches ref d = (shifted, newRemaining, spos)
    where matches      = map ((,) <$> matchScanner ref <*> id) d
          filtered     = filter ((>= 12) . matchCount . fst) matches
          shifted      = map ((\(d, o, _, _) -> map (subPos o) d) . fst) filtered
          spos         = map ((\(_, _, p, _) -> p) . fst) filtered
          newRemaining = map snd . filter ((< 12) . matchCount . fst) $ matches

matchScanner :: ScannerData -> ScannerData -> MatchData
matchScanner d1 d2 = maximumBy (comparing matchCount) bestMatches
    where refOffsets   = offsetData d1
          matchOffsets = map offsetData . rotateScanner $ d2
          matches      = map (matchData <$> refOffsets <*>) matchOffsets
          bestMatches  = map (maximumBy (comparing matchCount)) matches

matchCount :: MatchData -> Int
matchCount (_, _, _, n) = n

matchData :: RelativeData -> RelativeData -> MatchData
matchData (d1, o1) (d2, o2) = (d2, o1, subPos o1 o2, length $ intersect d1 d2)

offsetData :: ScannerData -> [RelativeData]
offsetData d = map ((,) <$> (\x -> map (subPos x) d) <*> id) d

rotateScanner :: ScannerData -> [ScannerData]
rotateScanner d = map (`map` d) rotations

rotations :: [Position -> Position]
rotations = nubBy (\x y -> x (1, 2, 3) == y (1, 2, 3)) candidates
    where candidates     = (!! 4) $ iterate ((.) <$> allRot <*>) [id]
          allRot         = [id, rotX, rotY, rotZ]
          rotX (x, y, z) = ( x,  z, -y)
          rotY (x, y, z) = (-z,  y,  x)
          rotZ (x, y, z) = ( y, -x,  z)

addPos :: Position -> Position -> Position
addPos (x1, y1, z1) (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)

subPos :: Position -> Position -> Position
subPos (x1, y1, z1) (x2, y2, z2) = (x1 - x2, y1 - y2, z1 - z2)

manhattan :: Position -> Position -> Int
manhattan (x1, y1, z1) (x2, y2, z2) = abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)

parse :: [String] -> [ScannerData]
parse = map (parseScanner . tail) . splitOn [""]
    where parseScanner         = map (makeTriple . map read . splitOn ",")
          makeTriple (x:y:z:_) = (x, y, z)
