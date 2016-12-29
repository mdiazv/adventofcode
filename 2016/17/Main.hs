module Main where

import Data.Digest.Pure.MD5 (md5)
import Data.List (maximumBy)
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import qualified Data.ByteString.Lazy.Char8 as C8

main :: IO ()
main = do
  solve "ihgpwlah"
  solve "kglvqrro"
  solve "ulqzkmiv"
  solve "vkjiggvb"

solve :: Route -> IO ()
solve key = do
  let routes = bfs key
  putStrLn $ "Key: " ++ key
  putStrLn $ "Shortest route: " ++ head routes
  putStrLn $ "Length of longest route: " ++ show (length $ longest routes)

type Route = String
type Pos = (Int, Int)
type State = (Pos, Route)
data Doors = Doors { up, down, left, right :: Bool }
           deriving (Show)

class Queue q where
  empty :: q a -> Bool
  get :: q a -> Maybe (a, q a)
  ins :: a -> q a -> q a
  insl :: q a -> [a] -> q a
  insl = foldr ins

data Fifo a = F [a] [a]

instance Queue Fifo where
  empty (F xs ys) = null xs && null ys
  ins y (F xs ys) = F xs (y:ys)
  get (F []     []) = Nothing
  get (F (x:xs) ys) = Just (x, F xs ys)
  get (F []     ys) = get (F (reverse ys) [])

longest :: [Route] -> Route
longest = maximumBy (comparing length)

bfs :: Route -> [Route]
bfs passwd = map (\xs -> drop (length passwd) $ reverse . snd $ xs) $ bfs' (3, 3) $ F [((0, 0), reverse passwd)] []

bfs' :: Pos -> Fifo State -> [State]
bfs' end queue = 
  case get queue of
    Nothing -> []
    Just (next@(pos, route), queue') ->
      if pos == end
        then next : (bfs' end queue')
        else bfs' end $ insl queue' $ adjacent next

adjacent :: State -> [State]
adjacent st@(pos, route) = mapMaybe (walk st) "URDL"

walk :: State -> Char -> Maybe State
walk (p@(i, j), route) c =
  if inBounds p' && canPass (openDoors route) c
    then Just (p', c:route)
    else Nothing
  where
    p' = move p c

    inBounds :: Pos -> Bool
    inBounds (i, j) = i >= 0 && j >= 0 && i < 4 && j < 4

    move :: Pos -> Char -> Pos
    move (i, j) c
      | c == 'U' = (i-1, j)
      | c == 'D' = (i+1, j)
      | c == 'L' = (i, j-1)
      | c == 'R' = (i, j+1)

    canPass :: Doors -> Char -> Bool
    canPass ds c
      | c == 'U' = up ds
      | c == 'D' = down ds
      | c == 'L' = left ds
      | c == 'R' = right ds

openDoors :: Route -> Doors
openDoors route =
  let (u:d:l:r:_) = show . md5 . C8.pack $ reverse route
  in  Doors { up = isOpen u, right = isOpen r, down = isOpen d, left = isOpen l }
  where
    isOpen :: Char -> Bool
    isOpen = (`elem` "bcdef")
