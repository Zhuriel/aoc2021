module Main where

main :: IO ()
main = do
    input <- readFile "input.txt"
    print . (\(x, y) -> x * y) . foldl execInstr (0, 0) $ lines input
    print . (\(x, y, _) -> x * y) . foldl execInstr2 (0, 0, 0) $ lines input

execInstr :: (Int, Int) -> String -> (Int, Int)
execInstr (x, y) instr = (x + xMovement, y + yMovement)
    where
        (xMovement, yMovement) = parseMovement instr

execInstr2 :: (Int, Int, Int) -> String -> (Int, Int, Int)
execInstr2 (x, y, aim) instr = (x + xMovement, y + yMovement, aim + aimChange)
    where
        (xMovement, aimChange) = parseMovement instr
        yMovement = aim * xMovement

parseMovement :: String -> (Int, Int)
parseMovement str
    | dir == "forward"  = (read num, 0)
    | dir == "down"     = (0, read num)
    | dir == "up"       = (0, - read num)
    | otherwise         = (0, 0)
    where
        dir:num:_ = words str
