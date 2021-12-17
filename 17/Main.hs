module Main where
import Data.List
import Data.List.Split
import Data.Ord
import Control.Monad
import Control.Monad.State
import Control.Monad.Loops

main :: IO ()
main = do
    input <- readFile "input.txt"
    let targetArea = parse input
    print . partA $ targetArea
    print . partB $ targetArea

partA :: TargetArea -> Int
partA ta = maximum . map (maximum . map snd) $ validPaths
    where validPaths  = filter (any $ inTargetArea ta) paths
          paths       = map (getPath ta) initialVels
          initialVels = (,) <$> [0..100] <*> [-200..500]

partB :: TargetArea -> Int
partB ta = length validPaths
    where validPaths  = filter (any $ inTargetArea ta) paths
          paths       = map (getPath ta) initialVels
          initialVels = (,) <$> [0..100] <*> [-200..500]

type Position   = (Int, Int)
type TargetArea = ((Int, Int), (Int, Int))
data ProbeState = PS { vel :: (Int, Int)
                     , pos :: Position
                     }
    deriving Show

getPath :: TargetArea -> (Int, Int) -> [Position]
getPath ta v = evalState (doSim ta) $ initialState v

doSim :: TargetArea -> State ProbeState [Position]
doSim ta = unfoldWhileM (not . overshot ta) simStep

simStep :: State ProbeState Position
simStep = do
    state <- get
    let (vx, vy) = vel state
        (x, y)   = pos state
        newPos   = (x + vx, y + vy)
        newVel   = (if vx == 0 then 0 else vx - 1, vy - 1)
    put PS {vel=newVel, pos=newPos}
    return newPos

inTargetArea :: TargetArea -> Position -> Bool
inTargetArea ((x1, y1), (x2, y2)) (x, y) =
    x >= x1 && x <= x2 && y >= y1 && y <= y2

overshot :: TargetArea -> Position -> Bool
overshot ((_, y1), (x2, _)) (x, y) = x > x2 || y < y1

initialState :: (Int, Int) -> ProbeState
initialState v = PS {vel=v, pos=(0, 0)}

parse :: String -> TargetArea
parse s = ((read x1, read y1), (read x2, read y2))
    where (_:xs:_:ys:_) = splitOneOf ",=" s
          (x1:x2:_)     = splitOn ".." xs
          (y1:y2:_)     = splitOn ".." ys
