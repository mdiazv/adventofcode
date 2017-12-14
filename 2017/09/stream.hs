import Data.Char
import Test.HUnit

main = do
  runTestTT tests
  input <- getContents
  putStrLn . show $ part1 input
  putStrLn . show $ part2 input

part1 = score . init
part2 = snd . dropGarbage . init

testNoGarbage = TestCase $ assertEqual "No Garbage, no removal" (dropGarbage "{}") ("{}", 0)
testEmptyGarbage = TestCase $ assertEqual "Empty Garbage is removed" (dropGarbage "{<X>}") ("{}", 1)
testRandomCharacters = TestCase $ assertEqual "Random characters are removed" (dropGarbage "{<random characters>}") ("{}", 17)
testNoGarbageNesting = TestCase $ assertEqual "Garbage is not nested" (dropGarbage "{<<<<>}") ("{}", 3)
testIgnoreGarbageClose = TestCase $ assertEqual "Ignore garbage close" (dropGarbage "{<{!>}>}") ("{}", 2)
testIgnoreIgnore = TestCase $ assertEqual "Ignore ignore" (dropGarbage "{<!!>}") ("{}", 0)
testIgnoreFrenzy = TestCase $ assertEqual "Ignore ignore and close" (dropGarbage "{<!!!>>}") ("{}", 0)
testComplexGarbage = TestCase $ assertEqual "Handle complex garbage" (dropGarbage "{<{o\"i!a,<{i<a>}") ("{}", 10)
testCrossGroupGarbage = TestCase $ assertEqual "Handle garbage across groups" (dropGarbage "{{<!>},{<!>},{<!>},{<a>}}") ("{{}}", 13)
testGroupedGarbage = TestCase $ assertEqual "Handle garbage inside groups" (dropGarbage "{{<a>},{<a>},{<a>},{<a>}}") ("{{},{},{},{}}", 4)

testNoGroups = TestCase $ assertEqual "No groups, no score" (score "") 0
testSingleGroup = TestCase $ assertEqual "One group, one point" (score "{}") 1
testSimpleNesting = TestCase $ assertEqual "Nested groups score higher" (score "{{{}}}") 6
testAlternateNesting = TestCase $ assertEqual "Nesting levels score different" (score "{{{},{},{{}}}}") 16
testGroupFullOfGarbage = TestCase $ assertEqual "A group consisting of little garbages" (score "{<a>,<a>,<a>,<a>}") 1
testGroupsAndGarbage = TestCase $ assertEqual "Group with groups of garbage" (score "{{<ab>},{<ab>},{<ab>},{<ab>}}") 9
testGroupsAndIgnoredGarbage = TestCase $ assertEqual "Group with groups of garbage with ignores" (score "{{<!!>},{<!!>},{<!!>},{<!!>}}") 9
testGroupsAndLargeIgnoredGarbage = TestCase $ assertEqual "Group with a large garbage with ignores" (score "{{<a!>},{<a!>},{<a!>},{<ab>}}") 3

dropGarbageTests = TestLabel "dropGarbage Tests" $
  TestList [ TestLabel "testNoGarbage" testNoGarbage
           , TestLabel "testEmptyGarbage" testEmptyGarbage
           , TestLabel "testRandomCharacters" testRandomCharacters
           , TestLabel "testNoGarbageNesting" testNoGarbageNesting
           , TestLabel "testIgnoreGarbageClose" testIgnoreGarbageClose
           , TestLabel "testIgnoreIgnore" testIgnoreIgnore
           , TestLabel "testIgnoreFrenzy" testIgnoreFrenzy
           , TestLabel "testComplexGarbage" testComplexGarbage
           , TestLabel "testCrossGroupGarbage" testCrossGroupGarbage
           , TestLabel "testGroupedGarbage" testGroupedGarbage
           ]

scoreTests = TestLabel "score Tests" $
  TestList [ TestLabel "testNoGroups" testNoGroups
           , TestLabel "testSingleGroup" testSingleGroup
           , TestLabel "testSimpleNesting" testSimpleNesting
           , TestLabel "testAlternateNesting" testAlternateNesting
           , TestLabel "testGroupFullOfGarbage" testGroupFullOfGarbage
           , TestLabel "testGroupsAndGarbage" testGroupsAndGarbage
           , TestLabel "testGroupsAndIgnoredGarbage" testGroupsAndIgnoredGarbage
           , TestLabel "testGroupsAndLargeIgnoredGarbage" testGroupsAndLargeIgnoredGarbage
           ]

tests = TestList [dropGarbageTests, scoreTests]

score :: String -> Int
score s = snd $ foldl score' (1, 0) $ fst $ dropGarbage s
  where
    score' :: (Int, Int) -> Char -> (Int, Int)
    score' (depth, s) c =
      case c of
        '{' -> (depth + 1, s + depth)
        '}' -> (depth - 1, s)
        ',' -> (depth, s)
        _   -> error ("Unexpected character: " ++ [c] ++ " (" ++ show (ord c) ++ ")")

dropGarbage :: String -> (String, Int)
dropGarbage s = dropGarbage' s False (("" ++), 0)
  where
    dropGarbage' :: String -> Bool -> ((String -> String), Int) -> (String, Int)
    dropGarbage' []     _         (acc, n) = (acc "", n)
    dropGarbage' (c:cs) inGarbage (acc, n) =
      case c of
        '<' -> dropGarbage' cs True (acc, n')
        '!' -> dropGarbage' (tail cs) True (acc, n)
        '>' -> dropGarbage' cs False (acc, n)
        _   -> dropGarbage' cs inGarbage (acc', n')
      where
        n' = if inGarbage then n+1 else n
        acc' = if inGarbage then acc else ((acc [c]) ++)
