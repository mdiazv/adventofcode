import Control.Applicative

main = do
  input <- getContents
  let m = map (map read . words) $ lines input :: [[Int]]
  putStrLn . show $ part1 m
  putStrLn . show $ part2 m

part1 = checksum minMaxChecksum
part2 = checksum divModChecksum

checksum :: ([Int] ->Int) -> [[Int]] -> Int
checksum rowChecksum m = sum $ map rowChecksum m

minMaxChecksum :: [Int] -> Int
minMaxChecksum r = leMax - leMin
  where
    leMin = minimum r
    leMax = maximum r

divModChecksum :: [Int] -> Int
divModChecksum r = sum $ divMod <$> r <*> r
  where
    divMod :: Int -> Int -> Int
    divMod a b
      | a /= b && a `mod` b == 0 = a `div` b
      | otherwise                = 0
