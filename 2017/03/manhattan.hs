import qualified Data.Map as M
import Control.Monad.State
import Prelude hiding (Left, Right)

type Point = (Int, Int)
type Delta = (Int, Int)
type Grid = [Point]
type Memory = M.Map Point Int

input :: Int
input = 277678

main = do
  putStrLn . show $ part1 input
  putStrLn . show $ part2 input

part1 input = dist $ head $ drop (input - 1) order
part2 input = firstGreater input

firstGreater :: Int -> Int
firstGreater n = fillUntilGreater (M.fromList [((0, 0), 1)]) $ drop 1 order
  where
    fillUntilGreater :: Memory -> Grid -> Int
    fillUntilGreater mem (p:ps) =
      if nextVal > n
        then nextVal
        else fillUntilGreater mem' ps
      where
        nextVal = sumNeighbors mem p
        mem' = M.insert p nextVal mem

sumNeighbors :: Memory -> Point -> Int
sumNeighbors mem (i, j) = sum [M.findWithDefault 0 nb mem | nb <- nbs]
  where
    nbs = [(i + di, j + dj) | di <- [-1, 0, 1], dj <- [-1, 0, 1]]

dist :: Point -> Int
dist (a, b) = abs a + abs b

order :: Grid
order = [(0, 0)] ++ concat [addSquare (-x, x) n | (x, n) <- zip [1 ..] [2, 4 ..]]

addSquare :: Point -> Int -> Grid
addSquare lowerRight n =
  walk lowerRight ( 1,  0) n ++
  walk upperRight ( 0, -1) n ++
  walk upperLeft  (-1,  0) n ++
  walk lowerLeft  ( 0,  1) n
  where
    upperRight = (fst lowerRight + n, snd lowerRight)
    upperLeft  = (fst upperRight, snd upperRight - n)
    lowerLeft  = (fst upperLeft - n, snd upperLeft)

    walk :: Point -> Delta -> Int -> Grid
    walk start (di, dj) n = take n $ drop 1 $ iterate step start
      where
        step :: Point -> Point
        step (i, j) = (i + di, j + dj)
