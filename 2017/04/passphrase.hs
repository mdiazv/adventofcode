import Data.List

main = do
  input <- getContents
  putStrLn . show $ part1 $ lines input
  putStrLn . show $ part2 $ lines input
  
part1 passwords = length $ filter isValidPassphrase $ passwords
part2 passwords = length $ filter isValidPassphraseAnagram $ passwords

isValidPassphrase :: String -> Bool
isValidPassphrase pass = noDuplicates $ words pass

isValidPassphraseAnagram :: String -> Bool
isValidPassphraseAnagram pass = noDuplicates $ map sort $ words pass

noDuplicates :: [String] -> Bool
noDuplicates xs = null $ filter (> 1) $ map length $ group $ sort xs
