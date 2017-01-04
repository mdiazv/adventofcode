module Main where

import Data.List (sort)

main :: IO ()
main = do
  input <- getContents
  let ranges = sort $ map parse $ lines input
  putStrLn . show $ firstAllowed ranges

type Range = (Int, Int)

firstAllowed :: [Range] -> Int
firstAllowed rs = snd $ foldl augment ((-1, -1), 0) rs

augment :: (Range, Int) -> Range -> (Range, Int)
augment ((low, hi), n) (newlow, newhi)
  | newlow <= n = ((low, max hi newhi), (max hi newhi)+1)
  | otherwise = ((-1, -1), n)

parse :: String -> Range
parse line = (read a, read b)
  where
    (a, '-':b) = span (/='-') line
