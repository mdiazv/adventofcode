module Main where

import Data.Char

main :: IO ()
main = do
  input <- getContents
  putStrLn . show . length $ filter isTriangle $ lines input
  putStrLn . show . length $ filter validTriangle $ makeColumnTriangles $ lines input

isTriangle :: String -> Bool
isTriangle = validTriangle . map read . words

validTriangle :: [Int] -> Bool
validTriangle [a, b, c] = a + b > c && a + c > b && b + c > a

makeColumnTriangles :: [String] -> [[Int]]
makeColumnTriangles lines =
  case splitAt 3 lines of
    ([], []) -> []
    (_3ls, lines') -> makeTriangles (map (map read . words) _3ls) ++ makeColumnTriangles lines'
  where
    makeTriangles :: [[Int]] -> [[Int]]
    makeTriangles [l1, l2, l3] = zipWith3 (\a b c -> [a, b, c]) l1 l2 l3
