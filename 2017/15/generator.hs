import Data.Bits ((.&.), shiftL)

main = do
  input <- getContents
  let [ga, gb] = map read $ lines input :: [Generator]
  putStrLn . show $ part1 ga gb
  putStrLn . show $ part2 ga gb

part1 ga gb = length $ judge 40000000 ga gb
part2 ga gb = length $ judge  5000000 ga' gb'
  where
    ga' = setDivisor 4 ga
    gb' = setDivisor 8 gb

judge n ga gb = filter matches $ take n $ zip (gen ga) (gen gb)
  where
    gen g = tail $ iterate (generate g) (seed g)

data Generator = Generator Int Int Int Int -- prime, factor, divisor, seed
               deriving (Show, Read)

matches :: (Int, Int) -> Bool
matches (a, b) = a .&. m == b .&. m
  where m = (shiftL 1 16) - 1

generate :: Generator -> Int -> Int
generate (Generator m k d _) x = head $ filter (multipleOf d) $ tail $ iterate gen x
  where
    gen x = k * x `mod` m
    multipleOf d k = k `mod` d == 0

setDivisor :: Int -> Generator -> Generator
setDivisor d (Generator p f _ s) = Generator p f d s

seed :: Generator -> Int
seed (Generator _ _ _ s) = s
