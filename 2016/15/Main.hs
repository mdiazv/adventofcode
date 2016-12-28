module Main where

main :: IO ()
main = do
  input <- getContents
  let discs = map parse $ zip [1..] $ lines input
  let extra = Disc (1 + length discs) 0 11
  putStrLn . unlines $ map show $ discs
  putStrLn . show $ firstAlign discs
  putStrLn . show $ firstAlign $ discs ++ [extra]

type Time = Int
data Disc = Disc Int Int Int
  deriving (Show, Eq)

firstAlign :: [Disc] -> Time
firstAlign ds = head $ filter (\k -> all (== 0) $ map (posAt k) ds) [0..]

posAt :: Time -> Disc -> Int
posAt t (Disc k start slots) = (k + start + t) `mod` slots

parse :: (Int, String) -> Disc
parse (k, s) = Disc k (read start) (read npos)
  where
    (start: _) = split (`elem` ['0'..'9']) $ ws !! 11
    npos = ws !! 3
    ws = words s

split :: (Char -> Bool) -> String -> [String]
split f s = 
  let (a, s') = span f s
  in case s' of
    [] -> [a]
    (del:cs) -> a:(split f cs)
