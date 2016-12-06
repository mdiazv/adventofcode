module Main where

import Data.Maybe

main :: IO ()
main = do
  input <- getContents
  putStrLn $ solve (Conf forbidden1 digits1) $ lines input
  putStrLn $ solve (Conf forbidden2 digits2) $ lines input

type Pos = (Int, Int)
type Move = Char
data Conf = Conf [Pos] [(Pos, Char)]

start :: Pos
start = (1, 1)

{- Conf data for part 1 -}
forbidden1 :: [Pos]
forbidden1 = [
         (0,-1), (1,-1), (2,-1),
 (-1,0),                       (3, 0),
 (-1,1),                       (3, 1),
 (-1,2),                       (3, 2),
          (0,3),  (1,3),  (2,3)]

digits1 :: [(Pos, Char)]
digits1 = [
        ((0,0), '1'), ((1,0), '2'), ((2,0), '3'),
        ((0,1), '4'), ((1,1), '5'), ((2,1), '6'),
        ((0,2), '7'), ((1,2), '8'), ((2,2), '9')]

{- Conf data for part 2 -}
forbidden2 :: [Pos]
forbidden2 = [   (2,-1),
            (1,0),     (3,0),
       (0,1),               (4,1),
 (-1,2),                          (5,2),
       (0,3),               (4,3),
            (1,4),     (3,4),
                 (2, 5)]

digits2 :: [(Pos, Char)]
digits2 = [
                             ((2,0), '1'),
               ((1,1), '2'), ((2,1), '3'), ((3,1), '4'),
 ((0,2), '5'), ((1,2), '6'), ((2,2), '7'), ((3,2), '8'), ((4,2), '9'),
               ((1,3), 'A'), ((2,3), 'B'), ((3,3), 'C'),
                             ((2,4), 'D')]


move :: [Pos] -> Pos -> Move -> Pos
move invalid p@(x, y) m =
  case m of
    'U' -> trymove (0,-1)
    'D' -> trymove (0, 1) 
    'L' -> trymove (-1,0)
    'R' -> trymove ( 1,0)
  where
    trymove (dx, dy) =
      let r = (x+dx, y+dy)
      in if r `elem` invalid then p else r


solve :: Conf -> [[Move]] -> String
solve (Conf forbidden digits) = reverse . snd . foldl dialNumber (start, "")
  where
    follow :: Pos -> [Move] -> Pos
    follow = foldl (move forbidden)

    dialNumber :: (Pos, String) -> [Move] -> (Pos, String)
    dialNumber (p, r) ms = (p', (fromJust $ lookup p' digits):r)
      where p' = follow p ms
