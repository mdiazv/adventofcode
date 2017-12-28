import qualified Data.Array.Unboxed as A
import Data.List.Split (splitOn)
import Data.List (findIndex, foldl')
import Data.Maybe (fromJust)

main = do
  input <- getContents
  let [n, rawmoves] = lines input
  let moves = map parseMove $ splitOn "," rawmoves
  putStrLn . show $ part1 (read n) moves
  putStrLn . show $ part2 (read n) moves

part1 n = foldl' perform (stage n)
part2 n moves = head $ drop steps $ iterate dance (stage n)
  where
    cycle :: [Stage]
    cycle = initial : (takeWhile (/= initial) $ tail $ iterate dance initial)

    dance :: Stage -> Stage
    dance s = foldl' perform s moves

    initial = stage n
    steps = 1000000000 `mod` (length cycle)

type Stage = (Int, [Char])
data Move = Spin Int
          | Exchange Int Int
          | Partner Char Char
          deriving (Show, Eq)

stage :: Int -> Stage
stage n = (n, take n ['a'..'z'])

perform :: Stage -> Move -> Stage
perform stg@(len, dancers) m =
  case m of
    Spin n -> (len, b ++ a)
      where (a, b) = splitAt (len - n) dancers
    Exchange i j -> (len, A.elems $ arr A.// changes)
      where
        changes = [(i, arr A.! j), (j, arr A.! i)]
        arr = A.listArray (0, len-1) dancers :: A.UArray Int Char
    Partner a b -> perform stg $ Exchange i j
      where
        i = fromJust $ findIndex (== a) dancers
        j = fromJust $ findIndex (== b) dancers

takeR :: Int -> [a] -> [a]
takeR n l = go (drop n l) l
  where
    go [] r = r
    go (_:xs) (_:ys) = go xs ys

parseMove :: String -> Move
parseMove (cmd:params) =
  case cmd of
    's' -> Spin $ read params
    'x' -> Exchange (read a) (read b)
    'p' -> Partner (head a) (head b)
  where [a, b] = splitOn "/" params
