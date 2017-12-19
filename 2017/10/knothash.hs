import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Bits (xor)
import Data.Char (isSpace, ord)
import Data.List.Split (chunksOf)
import Numeric (showHex)
import Test.HUnit

main = do
  runTestTT tests
  input <- getContents
  putStrLn . show $ part1 input
  putStrLn . show $ part2 input

part1 = checksum . (hashRound $ newState 256) . intLengthSequence
part2 = knotHash . asciiLengthSequence

testVector = V.enumFromN 0 5

testGetEmptyRange = TestCase $ assertEqual "Empty range stays the same" (V.fromList []) (getRange 0 0 testVector)
testGetSingletonRange = TestCase $ assertEqual "One element range stays the same" (V.fromList [0]) (getRange 0 1 testVector)
testGetRegularRange = TestCase $ assertEqual "Small range without wraparound" (V.fromList [0, 1, 2]) (getRange 0 3 testVector)
testGetWrapAroundRange = TestCase $ assertEqual "Small range with wraparound" (V.fromList [4, 0, 1]) (getRange 4 3 testVector)

testReverseEmptyRange = TestCase $ assertEqual "Empty range stays the same" testVector (reverseRange 0 0 testVector)
testReverseSingletonRange = TestCase $ assertEqual "One element range stays the same" testVector (reverseRange 0 1 testVector)
testReverseRegularRange = TestCase $ assertEqual "Small range without wraparound" (V.fromList [2, 1, 0, 3, 4]) (reverseRange 0 3 testVector)
testReverseWrapAroundRange = TestCase $ assertEqual "Small range with wraparound" (V.fromList [0, 4, 2, 3, 1]) (reverseRange 4 3 testVector)

testSampleHashRound = TestCase $ assertEqual "Sample case provided" (HashState (V.fromList [3, 4, 2, 1, 0]) 4 4) (hashRound (newState 5) [3, 4, 1, 5])

testAsciiLengthSequence = TestCase $ assertEqual "Ascii length sequence parsing" [49,44,50,44,51,17,31,73,47,23] (asciiLengthSequence "1,2,3")

testEmptyStringHash = TestCase $ assertEqual "knotHash empty" "a2582a3a0e66e6e86e3812dcb672a272" $ knotHash $ asciiLengthSequence ""
testAoC2017Hash = TestCase $ assertEqual "knotHash empty" "33efeb34ea91902bb2f59c9920caa6cd" $ knotHash $ asciiLengthSequence "AoC 2017"
test123Hash = TestCase $ assertEqual "knotHash empty" "3efbe78a8d82f29979031a4aa0b16a9d" $ knotHash $ asciiLengthSequence "1,2,3"
test124Hash = TestCase $ assertEqual "knotHash empty" "63960835bcdc130f0b66d7ff4f6a5a8e" $ knotHash $ asciiLengthSequence "1,2,4"

getRangeTests = TestLabel "getRange Tests" $
  TestList [ TestLabel "testGetEmptyRange" testGetEmptyRange
           , TestLabel "testGetSingletonRange" testGetSingletonRange
           , TestLabel "testGetRegularRange" testGetRegularRange
           , TestLabel "testGetWrapAroundRange" testGetWrapAroundRange
           ]

reverseRangeTests = TestLabel "reverseRange Tests" $
  TestList [ TestLabel "testReverseEmptyRange" testReverseEmptyRange
           , TestLabel "testReverseSingletonRange" testReverseSingletonRange
           , TestLabel "testReverseRegularRange" testReverseRegularRange
           , TestLabel "testReverseWrapAroundRange" testReverseWrapAroundRange
           ]

knotHashTests = TestLabel "knotHash Tests" $
  TestList [ TestLabel "testEmptyStringHash" testEmptyStringHash
           , TestLabel "testAoC2017Hash" testAoC2017Hash
           , TestLabel "test123Hash" test123Hash
           , TestLabel "test124Hash" test124Hash
           ]

tests = TestList [getRangeTests, reverseRangeTests, testSampleHashRound, testAsciiLengthSequence, knotHashTests]

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
knotHash :: [Int] -> Hash
knotHash lengths = toHex denseHash
  where
    denseHash  = map xorAll $ chunksOf 16 $ V.toList $ hashVector sparseHash
    sparseHash = head $ drop 64 $ iterate round (newState 256)
    xorAll = foldl xor 0
    round st   = hashRound st lengths
    toHex = concat . (map to02Hex) 
    to02Hex x = leading ++ showHex x ""
      where leading = if x < 16 then "0" else ""

intLengthSequence :: String -> [Int]
intLengthSequence str = map (read . T.unpack) $ T.split (== ',') $ T.pack str

asciiLengthSequence :: String -> [Int]
asciiLengthSequence str = (map ord $ filter (/= '\n') str) ++ salt
  where 
    salt = [17, 31, 73, 47, 23]

checksum :: HashState -> Int
checksum (HashState { hashVector = v }) = (v V.! 0) * (v V.! 1)

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
