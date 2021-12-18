module Main where
import Control.Monad
import Control.Monad.State
import Data.Char
import Data.List
import Data.Maybe

main :: IO ()
main = do
    input <- readFile "input.txt"
    let inputs = map (evalState parse) $ lines input
    print $ partA inputs
    print $ partB inputs

partA :: [FishNumber] -> Int
partA = fishMagnitude . foldl1 addFish

partB :: [FishNumber] -> Int
partB nums = maximum . map fishMagnitude $ addFish <$> nums <*> nums

data FishNumber = I Int | P FishNumber FishNumber
    deriving (Eq, Show)

fishMagnitude :: FishNumber -> Int
fishMagnitude (I x)   = x
fishMagnitude (P l r) = 3 * fishMagnitude l + 2 * fishMagnitude r

canReduce :: Int -> FishNumber -> Bool
canReduce _ (I x)   | x >= 10 = True
canReduce 4 (P _ _)           = True
canReduce n (P l r)           = canReduce (n + 1) l || canReduce (n + 1) r
canReduce _ _                 = False

addLeft :: Int -> FishNumber -> FishNumber
addLeft a (I x)   = I (x + a)
addLeft a (P l r) = P (addLeft a l) r

addRight :: Int -> FishNumber -> FishNumber
addRight a (I x)   = I (x + a)
addRight a (P l r) = P l (addRight a r)

explodeStep :: Int -> FishNumber -> State Bool (FishNumber, (Int, Int))
explodeStep _ (I x)             = return (I x, (0, 0))
explodeStep 4 x@(P (I l) (I r)) = do
    done <- get
    if not done then do
        put True
        return (I 0, (l, r))
    else return (x, (0, 0))

explodeStep n (P l r)           = do
    (newL, (ll, lr)) <- explodeStep (n + 1) l
    (newR, (rl, rr)) <- explodeStep (n + 1) r
    return (P (addRight rl newL) (addLeft lr newR), (ll, rr))

doExplode :: FishNumber -> State Bool FishNumber
doExplode x = do
    (res, _) <- explodeStep 0 x
    return res

splitStep :: FishNumber -> State Bool FishNumber
splitStep (I x) | x < 10    = return (I x)
                | otherwise = do
    done <- get
    if not done then do
        put True
        let halfX = x `div` 2
        return (P (I halfX) (I (x - halfX)))
    else return (I x)

splitStep (P l r) = do
    newL <- splitStep l
    newR <- splitStep r
    return (P newL newR)

reduceStep :: FishNumber -> FishNumber
reduceStep x = evalState (doExplode x >>= splitStep) False

reduceFish :: FishNumber -> FishNumber
reduceFish = fromJust . find (not . canReduce 0) . iterate reduceStep

addFish :: FishNumber -> FishNumber -> FishNumber
addFish l r = reduceFish (P l r)

parse :: State String FishNumber
parse = do
    char <- readChar
    case char of
        '[' -> do
            first <- parse
            consumeChar ','
            second <- parse
            consumeChar ']'
            return $ P first second
        d | d `elem` "0123456789" ->
            return $ I (digitToInt d)
        _ ->
            return $ error "unexpected pattern in input"

consumeChar :: Char -> State String ()
consumeChar c = do
    char <- readChar
    when (char /= c) . return $ error "unexpected character in input string"

readChar :: State String Char
readChar = do
    cs <- get
    put $ tail cs
    return $ head cs
