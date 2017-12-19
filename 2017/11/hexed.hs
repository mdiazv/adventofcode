import qualified Data.Text as T
import Test.HUnit

main = do
  runTestTT tests
  input <- getContents
  let moves = init input :: String
  putStrLn . show $ part1 moves
  putStrLn . show $ part2 moves

part1 str = dist $ foldl add (0,0) $ map toCartesian $ parseList ',' str
part2 str = maximum $ map dist $ scanl add (0, 0) $ map toCartesian $ parseList ',' str

test3NE    = TestCase $ assertEqual "test ne,ne,ne" 3       $ part1 "ne,ne,ne"
test2NE2SW = TestCase $ assertEqual "test ne,ne,sw,sw" 0    $ part1 "ne,ne,sw,sw"
test2NE2S  = TestCase $ assertEqual "test ne,ne,s,s" 2      $ part1 "ne,ne,s,s"
testSESW   = TestCase $ assertEqual "test se,sw,se,sw,sw" 3 $ part1 "se,sw,se,sw,sw"
testSSSW   = TestCase $ assertEqual "test s,s,sw" 3         $ part1 "s,s,sw"
testNESE   = TestCase $ assertEqual "test ne,se" 2          $ part1 "ne,se"

tests = TestList [ TestLabel "test3NE" test3NE
                 , TestLabel "test2NE2SW" test2NE2SW
                 , TestLabel "test2NE2S" test2NE2S
                 , TestLabel "testSESW" testSESW
                 , TestLabel "testSSSW" testSSSW
                 , TestLabel "testNESE" testNESE
                 ]

type Point = (Int, Int)

dist :: Point -> Int
dist (x, y) = if signum x == signum y
                then abs (x + y)
                else max (abs x) (abs y)

add :: Point -> Point -> Point
add (a,b) (c,d) = (a+c,b+d)

toCartesian :: String -> Point
toCartesian s =
  case s of
    "n"  -> ( 0, 1)
    "s"  -> ( 0,-1)
    "ne" -> ( 1, 0)
    "sw" -> (-1, 0)
    "se" -> ( 1,-1)
    "nw" -> (-1, 1)
    _    -> error $ "Bad move: " ++ s

parseList :: Char -> String -> [String]
parseList sep str = map (T.unpack) $ T.split (== sep) $ T.pack str
