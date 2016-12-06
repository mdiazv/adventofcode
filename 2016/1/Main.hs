module Main where

import Data.List

main :: IO ()
main = do
  input <- getLine
  let steps = map parseMove $ words $ filter (/=',') input
  putStrLn . show $ distance (North, 0, 0) steps
--  putStrLn . show $ distance3 (North, 0, 0) steps
  putStrLn . show $ distance2 (North, 0, 0) steps

type Pos = (Direction, Int, Int)
data Move = Move (Direction -> Direction) Int
data Direction = North | East | South | West
  deriving (Eq, Show)
data Answer = InProgress (Pos, [Pos]) | Solved (Pos, [Pos])
  deriving (Eq, Show)

right :: Direction -> Direction
right North = East
right East = South
right South = West
right West = North

left :: Direction -> Direction
left North = West
left West = South
left South = East
left East = North

vector :: Direction -> (Int, Int)
vector North = (0 , 1)
vector South = (0 ,-1)
vector East  = ( 1, 0)
vector West  = (-1, 0)

distance :: Pos -> [Move] -> Int
distance start moves = abs(x) + abs(y)
  where
    (_, x, y) = foldl move start moves

{- For solving part2 -}

baseAnswer :: Pos -> Answer
baseAnswer start = InProgress (start, [start])

distance3 :: Pos -> [Move] -> Answer
distance3 start moves = foldl visitedTwice (baseAnswer start) moves

distance2 :: Pos -> [Move] -> Int
distance2 start moves = abs(x) + abs(y)
  where
    base = InProgress (start, [start])
    Solved ((_, x, y), _) = foldl visitedTwice (baseAnswer start) moves

visitedTwice :: Answer -> Move -> Answer
visitedTwice s@(Solved _) _ = s
visitedTwice (InProgress (p, visited)) m =
  let
    ps' = detailedMove p m
    (outcome, visited') = firstVisited ps' visited
  in case outcome of
    Nothing -> InProgress (last ps', visited')
    Just p' -> Solved (p', visited')

firstVisited :: [Pos] -> [Pos] -> (Maybe Pos, [Pos])
firstVisited [] visited = (Nothing, visited)
firstVisited (p:ps) visited =
  if beenThere p visited
  then (Just p, visited)
  else firstVisited ps (p:visited)

beenThere :: Pos -> [Pos] -> Bool
beenThere (_, x, y) = any (\(_, px, py) -> x == px && y == py)

detailedMove :: Pos -> Move -> [Pos]
detailedMove (d,x,y) (Move rot n) =
  let
    d' = rot d
    rotated = (d', x, y)
    (dx, dy) = vector d'
  in tail $ scanl (\(_, px, py) _ -> (d', px+dx, py+dy)) rotated [1..n]

{- For solving part1 -}
move :: Pos -> Move -> Pos
move (d,x,y) (Move rot steps) = (d',x+x',y+y')
  where
    d' = rot d
    (x', y') = mapTuple (steps*) (vector d')

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (x, y) = (f x, f y)

parseMove :: String -> Move
parseMove (turn:steps) = Move (if turn == 'R' then right else left) (read steps)
