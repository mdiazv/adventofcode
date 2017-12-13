import qualified Data.Map as M
import Data.List (group, sort)
import Data.Maybe (fromJust, isJust)
import Control.Monad (msum)

main = do
  input <- getContents
  let ps = map parseProgram $ lines input :: [Program]
  let tree = buildTree ps
  putStrLn . show $ part1 tree
  putStrLn . show $ part2 tree

part1 = name . findRoot
part2 tree = findUnbalanced tree root
  where
    root = name $ findRoot tree

type ProgramID = String
type ProgramTree = M.Map ProgramID Program
type Unbalanced = (Int, ProgramID)
data Program = Program { name     :: ProgramID
                       , parent   :: Maybe ProgramID
                       , children :: [ProgramID]
                       , weight   :: Int
                       } deriving Show

findUnbalanced :: ProgramTree -> ProgramID -> Maybe Unbalanced
findUnbalanced tree pid
  -- If we have an answer pass it on
  | isJust childrenBalance = childrenBalance
  -- Children are balanced, keep searching
  | null missbehaving      = Nothing
  -- Children unbalanced
  | otherwise              = Just (correctWeight - (childWeight offendingChild), offendingChild)
  where
    childWeight pid = sum $ map (discWeight tree) (children $ program pid tree)
    childrenBalance = msum $ map (findUnbalanced tree) (children p)
    weights = sort $ [(discWeight tree c, c) | c <- (children p)]
    correctWeight = mode $ map fst weights
    missbehaving = filter (\x -> fst x /= correctWeight) weights
    offendingChild = snd $ head missbehaving
    p = program pid tree

mode :: [Int] -> Int
mode xs = snd $ maximum [(length x, head x) | x <- group $ sort xs]

discWeight :: ProgramTree -> ProgramID -> Int
discWeight tree pid = 
  let p = program pid tree
      cw = sum $ map (discWeight tree) (children p)
  in  (weight p) + cw

program :: ProgramID -> ProgramTree -> Program
program pid tree = fromJust $ M.lookup pid tree

findRoot :: ProgramTree -> Program
findRoot tree = climb tree (snd $ M.elemAt 0 tree)
  where
    climb :: ProgramTree -> Program -> Program
    climb tree p =
      case parent p of
        Nothing -> p
        Just p' -> climb tree (program p' tree)

buildTree :: [Program] -> ProgramTree
buildTree ps = foldr setParents base ps
  where
    toNamedTuple p = (name p, p)
    base = M.fromList $ map toNamedTuple ps

setParents :: Program -> ProgramTree -> ProgramTree
setParents p tree = foldr setOneParent tree $ children p
  where
    setOneParent :: ProgramID -> ProgramTree -> ProgramTree
    setOneParent = M.adjust (setParent $ name p)

    setParent :: ProgramID -> Program -> Program
    setParent id p = p { parent = Just id }

parseProgram :: String -> Program
parseProgram str = mkProgram id weight childIDs
  where
    (id : w : xs) = words str
    weight = read . init . tail $ w :: Int
    childIDs = parseChildren xs

    parseChildren :: [String] -> [ProgramID]
    parseChildren [] = []
    parseChildren (_ : cs) = map (remove ',') cs

    remove c str = [x | x <- str, x /= c]

    mkProgram :: ProgramID -> Int -> [ProgramID] -> Program
    mkProgram name weight children =
      Program { name = name
              , weight = weight
              , parent = Nothing
              , children = children
              }
