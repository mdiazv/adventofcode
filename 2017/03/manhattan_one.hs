
data Square = Square { dist   :: Integer
                     , base   :: Integer
                     , maxVal :: Integer
                     } deriving Show

main = do
  putStrLn . show $ manhattan 277678

manhattan :: Integer -> Integer
manhattan n = dist sq + centerDistance sq n
  where
    sq = findSquare n

findSquare :: Integer -> Square
findSquare n = head $ dropWhile (\sq -> maxVal sq < n) squares
  where
    squares :: [Square]
    squares = [mkSquare dist base | (dist, base) <- zip [0, 1 ..] [1, 3 ..]]

centerDistance :: Square -> Integer -> Integer
centerDistance sq n = minimum $ map (dist n) centers
  where
    centers = take 4 $ iterate (\x -> x - (base sq - 1)) (maxVal sq - base sq `div` 2)
    dist a b = abs $ a - b

mkSquare :: Integer -> Integer -> Square
mkSquare a b = Square {dist = a, base = b, maxVal = b*b}
