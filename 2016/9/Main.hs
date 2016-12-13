module Main where

import Data.Char (isSpace)

main :: IO ()
main = do
  input <- getContents
-- putStrLn . show $ uncompress input
  putStrLn . show . length $ uncompress input
  putStrLn . show $ countLength input

data Compressed = Text String | Marker Int Int String

uncompress :: String -> String
uncompress = p_text

countLength :: String -> Int
countLength = c_text

{- Part 2 -}
c_text :: String -> Int
c_text [] = 0
c_text (c:cs)
  | c == '('  = c_marker cs
  | isSpace c = c_text cs
  | otherwise = 1 + c_text cs

c_marker :: String -> Int
c_marker s =
  let
    (marker,close:cs) = span (/=')') s
    (len,(x:rep)) = span (/='x') marker
    (body,rest) = splitAt (read len) cs
    bodyCount = (read rep) * (c_text body)
  in
    bodyCount + c_text rest

{- Part 1 -}
p_text :: String -> String
p_text [] = []
p_text (c:cs)
  | c == '('  = p_marker cs
  | isSpace c = p_text cs
  | otherwise = c : (p_text cs)

p_marker :: String -> String
p_marker s =
  let
    (marker,close:cs) = span (/=')') s
    (len,(x:rep)) = span (/='x') marker
    (body,rest) = splitAt (read len) cs
  in
    (concat $ replicate (read rep) body) ++ p_text rest
