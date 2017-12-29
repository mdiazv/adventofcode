{-# LANGUAGE BangPatterns #-}

import Data.Maybe (fromJust)

main = do
  let n = 337
  putStrLn . show $ part1 n
  putStrLn . show $ part2 n

part1 n = head $ tail $ snd $ run (spin n) (1, [0]) 2017
part2 n = fromJust . action $ run (mockSpin n) (1, 0, Nothing) 50000000

type Spinlock = (Int, [Int])
type Fakelock = (Int, Int, Maybe Int)

action :: Fakelock -> Maybe Int
action (_, _, a) = a

mockSpin :: Int -> Fakelock -> Fakelock
mockSpin step (len, pos, !a) = (len + 1, pos', a')
  where
    pos' = ((pos + step) `mod` len) + 1
    a' = if pos' == 1
           then Just len
           else a

run :: (a -> a) -> a -> Int -> a
run f x n = head $ drop n $ iterate' f x

iterate' :: (a -> a) -> a -> [a]
iterate' f x = x' `seq` (x : iterate' f x')
  where x' = f x

spin :: Int -> Spinlock -> Spinlock
spin step (len, list) = (len + 1, list')
  where
    list' = len : (take len $ drop (step+1) $ cycle list)
