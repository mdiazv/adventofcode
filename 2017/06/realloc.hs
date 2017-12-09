import Data.Array.Unboxed
import Data.List
import Data.Set (member)
import qualified Data.Set as S

main = do
  input <- getContents
  let start = map read $ words input :: [Int]
  putStrLn . show $ part1 start
  putStrLn . show $ part2 start

type Memory = UArray Int Int

part1 start = (length $ takeUntilDuplicate $ countReallocs start) - 1
part2 start = p2 - p1
  where
    steps = takeUntilDuplicate $ countReallocs start
    repeated = last steps
    [p1, p2] = elemIndices repeated steps

takeUntilDuplicate :: Ord a => [a] -> [a]
takeUntilDuplicate xs = foldr go (const []) xs S.empty
  where
    go x cont set
      | x `member` set = [x]
      | otherwise      = x : cont (S.insert x set)

countReallocs :: [Int] -> [Memory]
countReallocs start = iterate realloc mem
  where
    (min, max) = (0, length start)
    mem = listArray (min, max-1) start

    realloc :: Memory -> Memory
    realloc mem = distribute (n+1) val newMem
      where
        (n, val) = maxValue mem
        newMem = mem // [(n, 0)]

    distribute :: Int -> Int -> Memory -> Memory
    distribute n val mem =
      if val > 0
        then distribute (n+1) (val-1) newMem
        else mem
      where
        i = n `mod` max
        newMem = mem // [(i, (mem ! i) + 1)]

    maxValue :: Memory -> (Int, Int)
    maxValue = findMax 0 (-1) minBound
      where
        findMax :: Int -> Int -> Int -> Memory -> (Int, Int)
        findMax i maxP maxV mem =
          if i >= max
            then (maxP, maxV)
            else
              if mem ! i > maxV
                then findMax (i+1) i (mem ! i) mem
                else findMax (i+1) maxP maxV mem
