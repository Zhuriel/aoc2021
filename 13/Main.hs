{-# LANGUAGE TupleSections #-}

module Main where
import Data.List
import Data.List.Split
import Data.Tuple
import Control.Monad

main :: IO ()
main = do
    input <- readFile "input.txt"
    let (dotsStr, _:instrsStr) = break null . lines $ input
    let dots = map parse dotsStr
    let instrs = map parseInstr instrsStr
    print $ partA dots (head instrs)
    putStrLn $ partB dots instrs

partA :: [Dot] -> Instr -> Int
partA dots instr = length . nub $ doFold dots instr

partB :: [Dot] -> [Instr] -> String
partB dots instrs = showArray $ foldl doFold dots instrs

type Dot = (Int, Int)
type Instr = (String, Int)

showArray :: [Dot] -> String
showArray dots = join . intersperse "\n" $ showLines
    where showLines = map (map charGen) coords
          coords    = map (\y -> map (,y) [0..maxX]) [0..maxY]
          maxX      = maximum . map fst $ dots
          maxY      = maximum . map snd $ dots
          charGen d | d `elem` dots = '#'
                    | otherwise     = '.'

doFold :: [Dot] -> Instr -> [Dot]
doFold dots ("x", x) = foldX x dots
doFold dots ("y", y) = foldY y dots
doFold _ _           = error "illegal fold direction"

foldX :: Int -> [Dot] -> [Dot]
foldX xf = map foldP
    where foldP (x, y) | x < xf    = (x, y)
                       | otherwise = (2 * xf - x, y)

foldY :: Int -> [Dot] -> [Dot]
foldY yf = map swap . foldX yf . map swap

parse :: String -> Dot
parse = (\(x:y:_) -> (x,y)) . map read . splitOn ","

parseInstr :: String -> Instr
parseInstr = readInstr . splitOn "=" . (!! 2) . words
    where readInstr (dir:amt:_) = (dir, read amt)
