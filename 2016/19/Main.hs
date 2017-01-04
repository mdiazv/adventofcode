module Main where

import Data.Sequence ((|>))
import qualified Data.Sequence as S

main :: IO ()
main = do
  let test = 5
  let real = 3014603
  solve "left" elephant test
  solve "left" elephant real
  solve "across" darkElephant test
  solve "across" darkElephant real

solve :: String -> (Int -> Int) -> Int -> IO ()
solve s f n = do
  putStrLn $ "Stealing from " ++ s ++ " with " ++ show n ++ " elves, all presents go to: " ++ show (f n)

elephant :: Int -> Int
elephant n = elephant' (S.fromList [1..n])

elephant' :: S.Seq Int -> Int
elephant' s
  | length s <= 2 = s `S.index` 0
  | otherwise     = elephant' $ (S.drop 2 s) |> (s `S.index` 0)

darkElephant :: Int -> Int
darkElephant n = darkElephant' $ S.splitAt (n `div` 2) (S.fromList [1..n])

darkElephant' :: (S.Seq Int, S.Seq Int) -> Int
darkElephant' (left, right)
  | rightLength == 1 = leftHead
  | rightLength >  leftLength = darkElephant' (left' |> rightSecond, right'' |> leftHead)
  | rightLength == leftLength = darkElephant' (left', right' |> leftHead)
  where
    leftLength = S.length left
    rightLength = S.length right
    leftHead = left `S.index` 0
    rightSecond = right `S.index` 1
    left'   = S.drop 1 left
    right'  = S.drop 1 right
    right''  = S.drop 2 right
