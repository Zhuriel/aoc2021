{-# LANGUAGE TupleSections #-}

module Main where
import Data.Char(digitToInt)
import Data.Ix
import Data.Array.IArray
import Data.List
import Control.Monad
import Control.Monad.State
import Control.Monad.Loops
import Debug.Trace

main :: IO ()
main = do
    input <- readFile "input.txt"
    let parsed = parseInput input
    putStrLn . showOctopi $ parsed
    putStrLn ""
    -- putStrLn . showOctopi $ execState (simStep) parsed
    -- putStrLn ""
    -- putStrLn . showOctopi . snd $ runState (sequence [simStep, simStep]) parsed
    print . partA $ parsed
    print . partB $ parsed

partA :: Octopi -> Int
partA = sum . evalState actions
    where actions = replicateM 100 simStep

partB :: Octopi -> Int
partB o = case elemIndex num states of
        Just n  -> n + 1
        Nothing -> -1
    where states = evalState (replicateM 1000 simStep) o
          num    = length . indices $ o

type Octopi = Array (Int, Int) Octopus
type Octopus = Maybe Int
type OctoState = State Octopi

simStep :: OctoState Int
simStep =
    do initialState <- get
       let is = indices initialState
       incrementOctopus is
       newState <- get
       put $ amap resetNothing newState
       return . foldr countNothing 0 $ elems newState
    where countNothing Nothing x = x + 1
          countNothing _       x = x
          resetNothing Nothing   = Just 0
          resetNothing x         = x

incrementOctopus :: [(Int, Int)] -> OctoState ()
incrementOctopus [] = return ()
incrementOctopus (i:is) =
    do curState <- get
       let curVal   = curState ! i
       put $ curState // [(i, newVal curVal)]
       let newIncrements = constr curState $ newIncr curVal
       incrementOctopus newIncrements
       incrementOctopus is
    where newVal (Just n) | n < 9   = Just (n + 1)
          newVal _                  = Nothing
          offsets                   = (,) <$> [-1,0,1] <*> [-1,0,1]
          newIncr (Just n) | n == 9 = map (addTuples i) offsets
          newIncr _                 = []
          addTuples (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
          constr s = filter (flip elem $ indices s)

showOctopi :: Octopi -> String
showOctopi o = join $ intersperse "\n" sLines
    where sLines               = map sLine [0..boundY]
          sLine y              = join $ map (\x -> oNum $ o!(x, y)) [0..boundX]
          (_,(boundX, boundY)) = bounds o
          oNum (Just n)        = show n
          oNum Nothing         = "x"

parseInput :: String -> Octopi
parseInput s = array bounds (join $ zipWith zip coords lists)
    where lists      = (map . map) (Just . digitToInt) $ lines s
          dimensions = (length lists - 1, (length . head $ lists) - 1)
          bounds     = ((0, 0), dimensions)
          coords     = map (\y -> map (,y) [0..]) [0..]
