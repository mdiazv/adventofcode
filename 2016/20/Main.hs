module Main where

import Data.List (sort)

main :: IO ()
main = do
  input <- getContents
  let ranges = sort $ map parse $ lines input
  putStrLn . show $ firstAllowed ranges
  putStrLn . show $ countGaps $ mergeRanges ranges

type Range = (Int, Int)

countGaps :: [Range] -> Int
countGaps rs = sum $ map gapSize $ zip rs (tail rs)

gapSize :: (Range, Range) -> Int
gapSize ((_, h1), (l2, _)) = l2 - h1 - 1

mergeRanges :: [Range] -> [Range]
mergeRanges [] = []
mergeRanges rs = bigrange : mergeRanges rs'
  where
    (bigrange, rs') = augmentRange (head rs) (tail rs)

augmentRange :: Range -> [Range] -> (Range, [Range])
augmentRange range [] = (range, [])
augmentRange range@(low, high) rss@((newlow, newhigh):rs)
  | newlow <= high+1 = augmentRange (low, max high newhigh) rs
  | otherwise = (range, rss)

firstAllowed :: [Range] -> Int
firstAllowed rs = foldl augment 0 rs

augment :: Int -> Range -> Int
augment n (newlow, newhi)
  | newlow <= n = max n (newhi+1)
  | otherwise = n

parse :: String -> Range
parse line = (read a, read b)
  where
    (a, '-':b) = span (/='-') line
