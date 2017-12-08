import Data.Array.Unboxed

main = do
  input <- getContents
  let offsets = map read $ lines input :: [Int]
  putStrLn . show $ part1 offsets
  putStrLn . show $ part2 offsets

type Maze = UArray Int Int

part1 = jump increaseOne
part2 = jump updateWeird

jump :: (Int -> Int) -> [Int] -> Int
jump updateFn offsets = jump' 0 0 maze
  where
    (min, max) = (0, length offsets)
    maze = listArray (min, max) offsets
    
    jump' :: Int -> Int -> Maze -> Int
    jump' n acc arr = 
      if n < min || n >= max
        then acc
        else jump' (n + val) (acc + 1) (arr // [(n, newVal)])
      where
        val = arr ! n
        newVal = updateFn val

increaseOne :: Int -> Int
increaseOne val = val + 1

updateWeird :: Int -> Int
updateWeird x = if x >= 3 then x - 1 else x + 1
