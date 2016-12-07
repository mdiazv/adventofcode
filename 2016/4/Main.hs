module Main where

import Data.Char
import Data.List

main :: IO ()
main = do
  input <- getContents
  let rooms = map toRoom $ lines input
  putStrLn . show $ sumValidRooms rooms
--  putStrLn . unlines $ map show $ decodeRooms rooms
  putStrLn . show . getSID . head $ filter (\r -> decodeName r == "northpole object storage") rooms

data Room = Room { getName :: String
                 , getSID  :: Int
                 , getCSum :: String
                 }

instance Show Room where
  show r = "Room " ++ (getName r) ++ " - " ++ show (getSID r) ++ " - " ++ (getCSum r)
                
toRoom :: String -> Room
toRoom s = Room { getName = init name, getSID = read sid, getCSum = csum }
  where
    (name, sid_csum) = span (`notElem` ['0'..'9']) s
    (sid, braced_csum) = span (`elem` ['0'..'9']) sid_csum
    csum = filter (`elem` ['a'..'z']) braced_csum
    
sumValidRooms :: [Room] -> Int
sumValidRooms = foldl' (+) 0 . map getSID . filter isValid

decodeRooms :: [Room] -> [Room]
decodeRooms = map (\r -> r { getName = decodeName r })

isValid :: Room -> Bool
isValid r = calcChecksum (filter (/='-') $ getName r) == getCSum r

calcChecksum :: String -> String
calcChecksum r = map snd $ take 5 $ sortBy sortGT freqTuples
  where
    freqTuples = map (\ls -> (length ls, head ls)) . group . sort $ r

decodeName :: Room -> String
decodeName r = map (rot $ getSID r) $ getName r

rot :: Int -> Char -> Char
rot n c
  | c `elem` ['a'..'z'] = chr $ (ord 'a') + ((ord c) - (ord 'a') + n) `rem` 26
  | c == '-' = ' '

sortGT (a1, b1) (a2, b2)
  | a1 < a2 = GT
  | a1 > a2 = LT
  | a1 == a2 = compare b1 b2

encrypted = toRoom "qzmt-zixmtkozy-ivhz-343-[zimth]"
