module Main where

import Control.Monad (forM_)
import Data.Bits (popCount)
import qualified Data.Map as M
import qualified Data.PQueue.Min as PQ

main :: IO ()
main = do
  let isOpenTest' = isOpen 10
  let isOpenPart1' = isOpen 1358
  let start = (1,1)
  let endTest = (7,4)
  let endPart1 = (31,39)
  let test = dijkstra isOpenTest' endTest start
  let part1 = dijkstra isOpenPart1' endPart1 start
  putStrLn . show $ test
  putStrLn . show $ part1
  putStrLn . show $ (length test) - 1
  putStrLn . show $ (length part1) - 1
--  drawMaze isOpenTest' 9 6
--  drawMaze isOpenPart1' 40 40

type Dist = Int
type Pos = (Int, Int)
type PosSet = M.Map Pos Bool
type PosPQ = PQ.MinQueue QueueEntry
type QueueEntry = (Dist, Pos, [Pos])

drawMaze :: (Pos -> Bool) -> Int -> Int -> IO ()
drawMaze isOpen x y =
  forM_ [0..y] $ \y -> do
    let line = map (render isOpen) [(x, y) | x <- [0..x]]
    putStrLn line

render :: (Pos -> Bool) -> Pos -> Char
render isOpen p = if isOpen p then '.' else '#'

dijkstra :: (Pos -> Bool) -> Pos -> Pos -> [Pos]
dijkstra isOpen end start = dijkstra' isOpen end (PQ.singleton (0, start, [])) M.empty

dijkstra' :: (Pos -> Bool) -> Pos -> PosPQ -> PosSet -> [Pos]
dijkstra' isOpen end open closed
  | start == end            = reverse $ start:path
  | start `M.member` closed = dijkstra' isOpen end open' closed
  | otherwise               = dijkstra' isOpen end augmented closed'
  where
    ((dist, start, path), open') = PQ.deleteFindMin open
    closed' = M.insert start True closed
    successors = filter (`M.notMember` closed) $ filter isOpen $ neighbors start
    augmented = foldr addOrUpdate open' successors

    addOrUpdate :: Pos -> PosPQ -> PosPQ
    addOrUpdate p pq =
      let
        (filtered, rest) = PQ.partition (\(_, pos, _) -> pos == p) pq
        (origDist, _, _) = PQ.findMin filtered
      in if PQ.null filtered
         then PQ.insert (dist+1, p, start:path) pq
         else if dist+1 < origDist
              then PQ.insert (dist+1, start, start:path) rest
              else pq

isOpen :: Int -> Pos -> Bool
isOpen secret (x, y)
  | x < 0 || y < 0 = False
  | otherwise = even nbits
  where
    nbits = popCount $ base + secret
    base = x*x + 3*x + 2*x*y + y + y*y

neighbors :: Pos -> [Pos]
neighbors (x, y) = [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]
