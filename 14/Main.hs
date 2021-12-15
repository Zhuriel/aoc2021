module Main where
import qualified Data.Map as M
import Data.Ord
import Data.List
import Control.Monad

main :: IO ()
main = do
    input <- readFile "input.txt"
    let ([startPoint],_:rulesStr) = break null . lines $ input
    let ruleMap = foldr parseRule M.empty rulesStr
    print $ partA ruleMap startPoint
    print $ partB ruleMap startPoint

partA :: Rules -> Polymer -> Int
partA m p = most - least
    where most  = length . longestList $ grouped
          least = length . shortestList $ grouped
          grouped = group . sort $ doPolymer m 10 p

partB :: Rules -> Polymer -> Int
partB m p = most - least
    where most  = maximum $ M.elems result
          least = minimum $ M.elems result
          result = doPolymerB m 40 p

shortestList :: [[a]] -> [a]
shortestList = minimumBy (comparing length)

longestList :: [[a]] -> [a]
longestList = maximumBy (comparing length)

type Polymer = String
type Rules = M.Map (Char, Char) Char
type PairsMap = M.Map (Char, Char) Int
type ElementsMap = M.Map Char Int

doPolymer :: Rules -> Int -> Polymer -> Polymer
doPolymer m n = (!! n) . iterate (insertStep m)

doPolymerB :: Rules -> Int -> Polymer -> ElementsMap
doPolymerB m n p = snd . (!! n) $ iterate (insertStepB m) initialState
    where initialPairs = makePairsMap p
          initialElems = foldr (\c -> incrElem (c, 1)) M.empty p
          initialState = (initialPairs, initialElems)

parseRule :: String -> Rules -> Rules
parseRule s = M.insert (l, r) result
    where (pairStr:_:resStr:_) = words s
          (l:r:_)              = pairStr
          (result:_)           = resStr

insertStep :: Rules -> Polymer -> Polymer
insertStep _ []       = []
insertStep _ [x]      = [x]
insertStep m (l:r:xs) = l : newChar : insertStep m (r:xs)
    where newChar = m M.! (l, r)

insertStepB :: Rules -> (PairsMap, ElementsMap) -> (PairsMap, ElementsMap)
insertStepB m (pairs, elements) = (newPairs m pairs, newElements)
    where newElements = foldr incrElem elements . M.toList $ addedElements m pairs

addedElements :: Rules -> PairsMap -> ElementsMap
addedElements m p = foldr incrElem M.empty mapped
    where mapped          = map newElems (M.toList p)
          newElems (c, v) = (m M.! c, v)

newPairs :: Rules -> PairsMap -> PairsMap
newPairs m p = foldr incrElem M.empty mapped
    where mapped = join . map newP $ M.toList p
          newP (k@(c1, c2), v) = [((c1, newC k), v), ((newC k, c2), v)]
          newC k = m M.! k

makePairsMap :: Polymer -> PairsMap
makePairsMap []  = M.empty
makePairsMap [_] = M.empty
makePairsMap (c1:c2:cs) = incrElem ((c1, c2), 1) (makePairsMap (c2:cs))

incrElem :: Ord k => (k, Int) -> M.Map k Int -> M.Map k Int
incrElem (k, n) = M.alter (incrMaybe n) k
    where incrMaybe n (Just x) = Just (x + n)
          incrMaybe n Nothing  = Just n
