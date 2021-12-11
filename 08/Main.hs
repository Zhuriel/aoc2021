module Main where
import Data.List.Split
import Data.List

main :: IO ()
main = do
    input <- readFile "input.txt"
    let parsedLines = map parseLine $ lines input
    print . partA . map snd $ parsedLines
    print $ partB parsedLines

partA :: [[DigitSignals]] -> Int
partA = sum . map (length . filter isUniqueDigit)

partB :: [([DigitSignals], [DigitSignals])] -> Int
partB = sum . map getLineResult

parseLine :: String -> ([DigitSignals], [DigitSignals])
parseLine s = (words patterns, words output)
    where (patterns:output:_) = splitOn "|" s

getLineResult :: ([DigitSignals], [DigitSignals]) -> Int
getLineResult (learn, output) = foldl (\x d -> 10 * x + d) 0 digits
    where digits  = map getDigitValue . map (mapSignals mapping) $ output
          mapping = determineMapping learn

type DigitSignals = String
type DigitSignalsMapped = [Bool]
type DigitSignalMapping = [Char]

getDigitValue :: DigitSignalsMapped -> Int
getDigitValue (True : True : True : False: True : True : True :_) = 0
getDigitValue (False: False: True : False: False: True : False:_) = 1
getDigitValue (True : False: True : True : True : False: True :_) = 2
getDigitValue (True : False: True : True : False: True : True :_) = 3
getDigitValue (False: True : True : True : False: True : False:_) = 4
getDigitValue (True : True : False: True : False: True : True :_) = 5
getDigitValue (True : True : False: True : True : True : True :_) = 6
getDigitValue (True : False: True : False: False: True : False:_) = 7
getDigitValue (True : True : True : True : True : True : True :_) = 8
getDigitValue (True : True : True : True : False: True : True :_) = 9
getDigitValue _ = -1000000

mapSignals :: DigitSignalMapping -> DigitSignals -> DigitSignalsMapped
mapSignals mapping signals = map (`elem` signals) mapping

isUniqueDigit :: DigitSignals -> Bool
isUniqueDigit s = length s `elem` [2, 3, 4, 7]

filterByLength :: Int -> [DigitSignals] -> [DigitSignals]
filterByLength n = filter ((==n) . length)

determineMapping :: [DigitSignals] -> DigitSignalMapping
determineMapping sigs = map (`determineSegment` sigs) [0..6]

numOccur :: [DigitSignals] -> [(Char, Int)]
numOccur sigs = map (\c -> (c, length $ filter (c `elem`) sigs)) "abcdefg"

determineSegment :: Int -> [DigitSignals] -> Char
-- segment 0 is in 7 but not 1, which are both unique length patterns
determineSegment 0 sig = filterNotIn sigs1 sigs7
    where sigs7 = head $ filterByLength 3 sig
          sigs1 = head $ filterByLength 2 sig

-- segment 1 is the only segment that occurs in exactly 6 patterns (045689)
determineSegment 1 sig = head . byNumOccurrences 6 $ sig

-- segment 2 is the last remaining so it is determined by what is left
determineSegment 2 sig = filterNotIn otherSegs "abcdefg"
    where otherSegs = map (`determineSegment` sig) [0, 1, 3, 4, 5, 6]

-- segment 3 is the center segment, since we have the horizonal 3 segments
-- we can use top and bottom (0 and 6) to find the center one
determineSegment 3 sig = filterNotIn segs06 $ middleSegs sig
    where segs06 = [determineSegment 0 sig, determineSegment 6 sig]

-- segment 4 is the only segment that occurs in exactly 4 patterns (0268)
determineSegment 4 sig = head . byNumOccurrences 4 $ sig

-- segment 5 is the only segment that occurs in exactly 9 patterns (013456789)
determineSegment 5 sig = head . byNumOccurrences 9 $ sig

-- segment 6 is the 3 horizontal segments excluding any that occur in
-- 7 and 4, which are both unique length patterns
determineSegment 6 sig = filterNotIn sigs47 $ middleSegs sig
    where sigs47 = head (filterByLength 4 sig) ++ head (filterByLength 3 sig)

determineSegment _ _   = 'x'

byNumOccurrences :: Int -> [DigitSignals] -> [Char]
byNumOccurrences n = map fst . filter ((== n) . snd) . numOccur

filterNotIn :: DigitSignals -> DigitSignals -> Char
filterNotIn sigs = head . filter (not . (`elem` sigs))

middleSegs :: [DigitSignals] -> DigitSignals
middleSegs = byNumOccurrences 3 . filterByLength 5
