module Knothash (knothash, Hash) where

import qualified Data.Vector as V
import Data.Bits (xor)
import Data.Char (isSpace, ord)
import Data.List.Split (chunksOf)
import Numeric (showHex)


knothash = knothash' . asciiLengthSequence

type Hash = String
data HashState = HashState { hashVector :: V.Vector Int
                           , hashIndex  :: Int
                           , hashSkip   :: Int
                           } deriving (Show, Eq)

newState :: Int -> HashState
newState size = HashState { hashVector = V.enumFromN 0 size
                          , hashIndex  = 0
                          , hashSkip   = 0
                          }

knothash' :: [Int] -> Hash
knothash' lengths = toHex denseHash
  where
    denseHash  = map xorAll $ chunksOf 16 $ V.toList $ hashVector sparseHash
    sparseHash = head $ drop 64 $ iterate round (newState 256)
    xorAll = foldl xor 0
    round st   = hashRound st lengths
    toHex = concat . (map to02Hex) 
    to02Hex x = leading ++ showHex x ""
      where leading = if x < 16 then "0" else ""

asciiLengthSequence :: String -> [Int]
asciiLengthSequence str = (map ord $ filter (/= '\n') str) ++ salt
  where 
    salt = [17, 31, 73, 47, 23]

hashRound :: HashState -> [Int] -> HashState
hashRound = foldl hashStep

hashStep :: HashState -> Int -> HashState
hashStep (HashState v i sk) k = HashState v' i' sk'
  where
    v'  = reverseRange i k v
    i'  = (i + k + sk) `mod` (V.length v)
    sk' = sk + 1

reverseRange :: Int -> Int -> V.Vector Int -> V.Vector Int
reverseRange i len v = V.update_ v idx range
  where
    range = V.reverse $ getRange i len v
    idx = V.fromList $ take len [x `mod` lv | x <- [i..]]
    lv = V.length v

getRange :: Int -> Int -> V.Vector Int -> V.Vector Int
getRange i len v = start V.++ end
  where
    start         = V.slice i wrap v
    end           = V.slice 0 rest v
    (wrap, rest)  = if i + len > lv then (lv - i, len - (lv - i)) else (len, 0)
    lv    = length v
