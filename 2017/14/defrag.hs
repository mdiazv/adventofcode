import qualified Data.Array as A
import qualified Data.Set as S
import Knothash (knothash, Hash)
import Numeric (readHex, showIntAtBase)
import Data.Bits (popCount)
import Data.Char (intToDigit, ord)

main = do
  key <- getLine
  let hashes = take 128 [knothash $ key ++ "-" ++ show k | k <- [0..127]]
  putStrLn . show $ part1 hashes
  putStrLn . show $ part2 $ disk key

part1 = sum . (map countBits)
part2 = length . regions

type Sector = (Int, Int)
type Region = S.Set Sector
type Disk = A.Array Sector Char

disk :: String -> Disk
disk key = A.listArray ((0, 0), (127, 127)) $ concat $ map toBitMap hashes
  where
    hashes = take 128 [knothash $ key ++ "-" ++ show k | k <- [0..127]]

regions :: Disk -> [Region]
regions disk = fst $ foldl addRegion ([], S.empty) $ A.indices disk
  where
    addRegion :: ([Region], Region) -> Sector -> ([Region], Region)
    addRegion (rs, closed) s =
      if s `S.member` closed || S.null r
        then (rs, closed)
        else ((r:rs), closed `S.union` r)
      where
        r = region s disk

region :: Sector -> Disk -> Region
region s disk = dfs S.empty s
  where
    dfs :: Region -> Sector -> Region
    dfs closed s = if s `S.member` closed || emptySector s
                     then closed
                     else foldl dfs closed' $ neighbors s
      where
        closed' = S.insert s closed
        neighbors (a, b) = filter inBounds $ [(a+1, b), (a, b+1), (a-1, b), (a, b-1)]
        emptySector s = (disk A.! s) == '0'

    inBounds (a, b) = la <= a && a <= ua && lb <= b && b <= ub
    ((la, lb), (ua, ub)) = A.bounds disk

toBitMap :: Hash -> [Char]
toBitMap = concat . (map hexToBitMap)

hexToBitMap :: Char -> [Char]
hexToBitMap c = takeRIdiomatic 4 $ "0000" ++ showIntAtBase 2 intToDigit (toInt c) ""

-- http://funktionale-programmierung.de/2013/08/01/haskell-imperativ.html
takeRIdiomatic :: Int -> [a] -> [a]
takeRIdiomatic n l = go (drop n l) l
  where
    go [] r = r
    go (_:xs) (_:ys) = go xs ys

countBits :: Hash -> Int
countBits = sum . (map hexToBitCount)

hexToBitCount :: Char -> Int
hexToBitCount c = popCount $ toInt c

toInt :: Char -> Int
toInt = fst . head . readHex . return
