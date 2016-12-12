module Main where

import Data.List (isPrefixOf)
import qualified Data.Map as M

main :: IO ()
main = do
  input <- getContents
  let display = foldl apply (M.fromList []) $ map parse $ lines input
  putStr $ render display
  putStrLn . show . length $ filter (=='#') $ render display

width, height :: Int
width = 50
height = 6

type Pixel = (Int, Int)
type Display = M.Map Pixel Bool

data Operation = Rectangle Int Int
               | RotateRow Int Int
               | RotateCol Int Int
               deriving (Show)

render :: Display -> String
render display = unlines $ 
  (flip map) [0..height-1] $ (\row ->
    (flip map) [0..width-1] $ (\col ->
      if (col,row) `M.member` display then '#' else '.'))

apply :: Display -> Operation -> Display
apply display op =
  case op of
    Rectangle a b -> M.union display rect
      where rect = M.fromList [((x, y), True) | x <- [0..a-1], y <- [0..b-1]]
    RotateRow row amount -> modifySome (\k v -> snd k == row) (rotateRow amount) display
    RotateCol col amount -> modifySome (\k v -> fst k == col) (rotateCol amount) display
  where
    modifySome :: (Pixel -> Bool -> Bool) -> (Pixel -> Pixel) -> Display -> Display
    modifySome partitionFn modifyFn display = M.union notAffected updated
      where (affected, notAffected) = M.partitionWithKey partitionFn display
            updated = foldl addElement M.empty $ map modifyFn $ M.keys affected

    addElement :: Display -> Pixel -> Display
    addElement display pixel = M.insert pixel True display

rotateCol, rotateRow :: Int -> Pixel -> Pixel
rotateRow n (x, y) = ((x+n) `mod` width, y)
rotateCol n (x, y) = (x, (y+n) `mod` height)

parse :: String -> Operation
parse s
  | "rect" `isPrefixOf` s = p_rect $ drop 5 s
  | "rotate row" `isPrefixOf` s = p_rotate RotateRow $ drop 2 $ words s
  | "rotate column" `isPrefixOf` s = p_rotate RotateCol $ drop 2 $ words s

p_rect :: String -> Operation
p_rect s = Rectangle a b
  where [a, b] = map read $ split (/='x') s

p_rotate :: (Int -> Int -> Operation) -> [String] -> Operation
p_rotate c s = c (read pos) amount
  where
    [_, pos] = split (/='=') (s !! 0)
    amount   = read $ s !! 2 :: Int

split :: (Char -> Bool) -> String -> [String]
split f s = 
  let (a, s') = span f s
  in case s' of
    [] -> [a]
    (del:cs) -> a:(split f cs)
