{-# LANGUAGE OverloadedStrings #-}

import Data.Char
import Control.Concurrent
import Data.List
import Data.Maybe
import Control.Monad
import Data.Digest.Pure.MD5
import qualified Data.ByteString.Lazy.Char8 as C8
import System.IO

passwordLength = 8

main :: IO ()
main = do
  forkIO animation
  key <- getLine
  let candidates = filter isPassword $ [show . md5 . C8.pack $ key ++ show n | n <- [3231928..]]
  putStrLn . show $ basicPassword candidates
  putStrLn . show $ gatherPassword candidates (0, [])
  putStrLn . show $ map snd $ gatherPassword candidates (0, [])

isPassword :: String -> Bool
isPassword s = take 5 s == "00000"

basicPassword :: [String] -> String
basicPassword = map (!! 5) . take passwordLength

gatherPassword :: [String] -> (Int, [(Int, Char)]) -> [(Int, Char)]
gatherPassword [] acc@(n,pass) = pass
gatherPassword (p:ps) acc@(n,pass)
  | n == passwordLength = sort pass
  | otherwise = case nextToken pass p of
                  Just x  -> gatherPassword ps (n+1,(x:pass))
                  Nothing -> gatherPassword ps acc

nextToken :: [(Int, Char)] -> String -> Maybe (Int, Char)
nextToken soFar p =
  let next = p !! 6
      posS = p !! 5
      pos  = (ord $ posS) - ord '0'
  in if posS `elem` ['0'..'7'] && isNothing (lookup pos soFar)
     then Just (pos, next)
     else Nothing

animation :: IO ()
animation = do
    mapM_ step ("\\|/-" :: String)
    animation

step :: Char -> IO ()
step c = do
  putStr ('\r':c:[])
  hFlush stdout
  threadDelay 500000
