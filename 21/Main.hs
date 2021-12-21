module Main where
import Data.List.Split
import Data.List
import Control.Monad
import Control.Monad.State
import Data.Maybe
import Debug.Trace

main :: IO ()
main = do
    input <- readFile "input.txt"
    let startPos = parse . lines $ input
    print . fst $ playGame 1000 startPos deterministicDie
    print $ diracDice 3
    --print $ partB startPos
    print $ branchGame 21 Player1 (startPos, (0, 0))

data Turn      = Player1 | Player2
    deriving Show
type GameState = State ((Int, Int), (Int, Int), Turn)
type GameTuple = ((Int, Int), (Int, Int))

newStates :: Turn -> GameTuple -> [(GameTuple, Int)]
newStates turn st = map ((,) <$> newState turn st . snd <*> fst) increments
    where increments = frequency . map sum $ diracDice 3
          newPos x n = ((x - 1 + n) `mod` 10) + 1

newState :: Turn -> GameTuple -> Int -> GameTuple
newState turn ((p1, p2), (s1, s2)) n = case turn of
    Player1 -> ((newPos p1 n, p2), (newScore s1 p1 n, s2))
    Player2 -> ((p1, newPos p2 n), (s1, newScore s2 p2 n))
    where newPos x inc = ((x - 1 + inc) `mod` 10) + 1
          newScore s x inc = s + newPos x inc

nextTurn :: Turn -> Turn
nextTurn Player1 = Player2
nextTurn Player2 = Player1

branchGame :: Int -> Turn -> GameTuple -> (Int, Int)
branchGame ws _ (_, (s1, _)) | s1 >= ws = (1, 0)
branchGame ws _ (_, (_, s2)) | s2 >= ws = (0, 1)
branchGame ws turn st = traceShowId $ foldr1 addTuple wins
    where newSt   = newStates turn st
          newWins = map ((,) <$> branchGame ws (nextTurn turn) . fst <*> snd) newSt
          wins    = map (multTuple <$> snd <*> fst) newWins

multTuple :: Int -> (Int, Int) -> (Int, Int)
multTuple s (x, y) = (x * s, y * s)

addTuple :: (Int, Int) -> (Int, Int) -> (Int, Int)
addTuple (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

playGame :: Int -> (Int, Int) -> [Int] -> (Int, Turn)
playGame winScore startPos rolls = (3 * (length turns + 1) * result, player)
    where initial   = (startPos, (0, 0), Player1)
          turnRolls = chunksOf 3 rolls
          game      = evalState (mapM (makeTurn winScore) turnRolls) initial
          (turns, (Just (result, player)):_) = break isJust game

diracDice :: Int -> [[Int]]
diracDice 0 = [[]]
diracDice n = (:) <$> [1, 2, 3] <*> diracDice (n - 1)

frequency :: Ord a => [a] -> [(Int,a)]
frequency list = map (\l -> (length l, head l)) (group (sort list))

deterministicDie :: [Int]
deterministicDie = join $ repeat [1..100]

makeTurn :: Int -> [Int] -> GameState (Maybe (Int, Turn))
makeTurn winScore rolls = do
    ((p1, p2), (s1, s2), turn) <- get
    let curPos     = case turn of Player1 -> p1
                                  Player2 -> p2
        curScore   = case turn of Player1 -> s1
                                  Player2 -> s2
        otherScore = case turn of Player1 -> s2
                                  Player2 -> s1
        newPos = ((curPos - 1 + sum rolls) `mod` 10) + 1
        newScore = curScore + newPos
    case turn of
        Player1 -> put ((newPos, p2), (newScore, s2), Player2)
        Player2 -> put ((p1, newPos), (s1, newScore), Player1)
    if newScore >= winScore then return (Just (otherScore, turn))
                        else return Nothing


parse :: [String] -> (Int, Int)
parse = (\x -> (read $ head x, read $ x!!1)) . map (last . words)
