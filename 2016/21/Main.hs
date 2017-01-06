module Main where

import Data.Foldable (toList)
import Data.List (permutations)
import Data.Maybe (fromJust)
import Data.Sequence ((><))
import qualified Data.Sequence as S

main :: IO ()
main = do
  input <- getContents
  let ops = map parse $ lines input
  putStrLn . show . last $ scramble "abcdefgh" ops
  putStrLn . show . head $ unscramble "fbgdceah" ops

type Passwd = S.Seq Char
data Operation = SwapPositions Int Int
               | SwapLetters Char Char
               | RotateN Int
               | RotatePos Char
               | ReverseRange Int Int
               | MoveLetter Int Int
               deriving (Show)

parse :: String -> Operation 
parse s =
  case ws of
    ("swap":"position":_) -> SwapPositions (int 2) (int 5)
    ("swap":"letter":_)   -> SwapLetters (char 2) (char 5)
    ("rotate":"based":_)  -> RotatePos (char 6)
    ("rotate":_)          -> RotateN ((dir 1) * (int 2))
    ("reverse":_)         -> ReverseRange (int 2) (int 4)
    ("move":_)            -> MoveLetter (int 2) (int 5)
  where
    ws = words s
    int n  = read $ ws !! n
    char n = head $ ws !! n
    dir n = if "right" == ws !! n
            then -1
            else 1

unscramble :: String -> [Operation] -> [String]
unscramble s ops = head $ filter match [scramble p ops | p <- permutations s]
  where
    match :: [String] -> Bool
    match ss = last ss == s

scramble :: String -> [Operation] -> [String]
scramble s ops = map toList $ scanl apply (S.fromList s) ops

apply :: Passwd -> Operation -> Passwd

apply s (SwapPositions x y) = S.update x atY $ S.update y atX $ s
  where
    atX = s `S.index` x
    atY = s `S.index` y

apply s (SwapLetters a b) = fmap replace s
  where
    replace :: Char -> Char
    replace c
      | c == a    = b
      | c == b    = a
      | otherwise = c

apply s (RotateN n) = bbb >< aaa
  where
    (aaa, bbb) = S.splitAt cut s
    cut = if n > 0
          then n `mod` len
          else (len - ((negate n) `mod` len)) `mod` len
    len = S.length s

apply s (RotatePos c) = foldl apply s ops
  where
    ops = [RotateN (-1), RotateN (negate i)] ++ extra
    extra = if i >= 4 then [RotateN (-1)] else []
    i = fromJust $ S.elemIndexL c s

apply s (ReverseRange x y) = start >< S.reverse range >< end
  where
    (start, tail) = S.splitAt x s
    (range, end)  = S.splitAt (y-x+1) tail

apply s (MoveLetter x y) = S.insertAt y atX $ S.deleteAt x $ s
  where
    atX = s `S.index` x
