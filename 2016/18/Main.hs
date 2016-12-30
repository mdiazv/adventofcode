module Main where

main :: IO ()
main = do
  solve 10 ".^^.^.^^^^"
  solve 40 ".^^^^^.^^^..^^^^^...^.^..^^^.^^....^.^...^^^...^^^^..^...^...^^.^.^.......^..^^...^.^.^^..^^^^^...^."
  solve 400000 ".^^^^^.^^^..^^^^^...^.^..^^^.^^....^.^...^^^...^^^^..^...^...^^.^.^.......^..^^...^.^.^^..^^^^^...^."

solve :: Int -> String -> IO ()
solve n s = do
  putStrLn $ "Within " ++ show n ++ " rows of: " ++ s
  putStrLn $ "Safe spots: " ++ show total
  where
    total = foldr (+) 0 $ map (length . (filter (=='.'))) $ take n $ iterate nextGen s

nextGen :: String -> String
nextGen s = map safeOrTrap $ zip3 ss (tail ss) (tail . tail $ ss)
  where ss = "." ++ s ++ "."

safeOrTrap :: (Char, Char, Char) -> Char
safeOrTrap ('^', '^', '.') = '^'
safeOrTrap ('.', '^', '^') = '^'
safeOrTrap ('^', '.', '.') = '^'
safeOrTrap ('.', '.', '^') = '^'
safeOrTrap _ = '.'
