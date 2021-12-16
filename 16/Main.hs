module Main where
import Numeric
import Data.Char
import Control.Monad
import Control.Monad.State

main :: IO ()
main = do
    input <- readFile "input.txt"
    let inputBin = hexToBin input
        packet   = evalState parsePacket inputBin
    print . sumPacketVer $ packet
    print . evalPacket $ packet

data Packet = Packet { ver      :: Int
                     , pktType  :: Int
                     , contents :: PacketContents }
    deriving (Show)
data PacketContents = Lit LiteralPacket | Op OperatorPacket
    deriving (Show)
type LiteralPacket  = Int
type OperatorPacket = [Packet]

sumPacketVer :: Packet -> Int
sumPacketVer Packet {ver=x, contents=(Lit _)} = x
sumPacketVer Packet {ver=x, contents=(Op op)} =
    x + (sum . map sumPacketVer $ op)

evalPacket :: Packet -> Int
evalPacket Packet {pktType=0, contents=(Op xs)} = sum . map evalPacket $ xs

evalPacket Packet {pktType=1, contents=(Op xs)} = product . map evalPacket $ xs

evalPacket Packet {pktType=2, contents=(Op xs)} = minimum . map evalPacket $ xs

evalPacket Packet {pktType=3, contents=(Op xs)} = maximum . map evalPacket $ xs

-- literals
evalPacket Packet {pktType=4, contents=(Lit x)} = x

-- comparisons
evalPacket Packet {pktType=5, contents=(Op (x:y:_))} =
    fromEnum (evalPacket x > evalPacket y)

evalPacket Packet {pktType=6, contents=(Op (x:y:_))} =
    fromEnum (evalPacket x < evalPacket y)

evalPacket Packet {pktType=7, contents=(Op (x:y:_))} =
    fromEnum (evalPacket x == evalPacket y)

-- fallback case
evalPacket p = error ("malformed packet: " ++ show p)

parsePacket :: State String Packet
parsePacket = do
    parsedVer  <- parseNumber 3
    parsedType <- parseNumber 3
    parsedContents <- parseContents parsedType
    return Packet { ver      = parsedVer
                  , pktType  = parsedType
                  , contents = parsedContents }

parseContents :: Int -> State String PacketContents
parseContents 4 = do Lit <$> parseLiteral 0
parseContents _ = do Op  <$> parseOperator

parsePacketsMaxLen :: Int -> State String [Packet]
parsePacketsMaxLen 0 = return []
parsePacketsMaxLen n | n < 0 = error "packets did not fit in specified length"
parsePacketsMaxLen n = do
    str <- get
    pkt <- parsePacket
    newStr <- get
    let pktLen = length str - length newStr
    pkts <- parsePacketsMaxLen (n - pktLen)
    return (pkt:pkts)

parseOperator :: State String OperatorPacket
parseOperator = do
    firstBit <- parseNumber 1
    if firstBit == 0 then do
        size <- parseNumber 15
        parsePacketsMaxLen size
    else do
        numPkts <- parseNumber 11
        replicateM numPkts parsePacket

parseLiteral :: Int -> State String LiteralPacket
parseLiteral n = do
    firstBit <- parseNumber 1
    curNum <- parseNumber 4
    if firstBit == 0 then return (n * 16 + curNum)
                     else parseLiteral $ n * 16 + curNum

parseNumber :: Int -> State String Int
parseNumber n = do
    str <- get
    let (toParse, rest) = splitAt n str
    put rest
    return . fst . head . readInt 2 (`elem` "01") digitToInt $ toParse

hexToBin :: String -> String
hexToBin = join . map binDigits
    where binDigits '0' = "0000"
          binDigits '1' = "0001"
          binDigits '2' = "0010"
          binDigits '3' = "0011"
          binDigits '4' = "0100"
          binDigits '5' = "0101"
          binDigits '6' = "0110"
          binDigits '7' = "0111"
          binDigits '8' = "1000"
          binDigits '9' = "1001"
          binDigits 'A' = "1010"
          binDigits 'B' = "1011"
          binDigits 'C' = "1100"
          binDigits 'D' = "1101"
          binDigits 'E' = "1110"
          binDigits 'F' = "1111"
          binDigits _   = "0000"
