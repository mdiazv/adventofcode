module Main where

main :: IO ()
main = do
  let test = "10000"
  let input = "11110010111001001"
  solve test 20
  solve input 272
  solve input 35651584

type Checksum = String
type Dragon = (Int, String)

solve :: String -> Int -> IO ()
solve input k = do
--  putStrLn . show $ dragonFill input k
  putStrLn . show $ checksum $ dragonFill input k

checksum :: String -> Checksum
checksum = until (odd . length) checksumStep

checksumStep :: Checksum -> Checksum
checksumStep [] = []
checksumStep (a:b:cs) = c : checksumStep cs
  where
    c = if a == b then '1' else '0'

dragonFill :: String -> Int -> String
dragonFill base n = truncate n $ until (atLeast n) dragonStep $ toDragon base
  where
    truncate :: Int -> Dragon -> String
    truncate n (_, s) = take n s

dragonStep :: Dragon -> Dragon
dragonStep (n, a) = (2*n+1, a ++ "0" ++ b)
  where
    b = map swap10 $ reverse a
    swap10 '0' = '1'
    swap10 '1' = '0'

toDragon :: String -> Dragon
toDragon s = (length s, s)

atLeast :: Int -> Dragon -> Bool
atLeast n (k, _) = k >= n
