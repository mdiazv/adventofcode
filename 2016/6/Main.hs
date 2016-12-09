module Main where

import Data.List
import Data.Ord
import qualified Data.Map as M

type FreqChar = (Int, Char)
type Code = M.Map Int String

main :: IO ()
main = do
  input <- getContents
  let freqs = foldl gatherCode M.empty $ lines input
  putStrLn . show $ decode (comparing Down) freqs
  putStrLn . show $ decode (comparing id) freqs

gatherCode :: Code -> String -> Code
gatherCode code s = foldl decodeChar code $ zip [0..] s

decodeChar :: Code -> (Int, Char) -> Code
decodeChar code (pos, c) = M.alter (addChar c) pos code

addChar :: Char -> Maybe String -> Maybe String
addChar c Nothing = Just [c]
addChar c (Just s) = Just (c:s)

decode :: (FreqChar -> FreqChar -> Ordering) -> Code -> String
decode cmp code = map snd $ sort . M.toList $ M.map (mostLikely cmp) code

mostLikely :: (FreqChar -> FreqChar -> Ordering) -> String -> Char
mostLikely cmp s = snd . head . sortBy cmp $ map (\s -> (length s, head s)) $ group $ sort s
