import qualified Data.Map as M
import Data.List (foldl')
import Data.Maybe (fromJust)

main = do
  input <- getContents
  let code = map parseInstruction $ lines input
  putStrLn . show $ part1 code
  putStrLn . show $ part2 code

part1 :: [Instruction] -> Int
part1 = maxValue . memory . (foldl' execute newState)
  where
    maxValue = M.foldr max minBound
part2 = maxVal . (foldl' execute newState)

type Memory = M.Map String Int
type Register = String
data ProgramState = ProgramState { memory :: Memory
                                 , maxVal :: Int
                                 } deriving Show
data Operator = Operator { operatorFn  :: Int -> Int -> Bool
                         , operatorStr :: String
                         }
data Condition = Condition Operator Register Int
data Instruction = Instruction { register       :: Register
                               , modifyByAdding :: Int
                               , condition      :: Condition
                               , instructionStr :: String
                               } deriving Show

instance Show Condition where
  show (Condition op reg val) =
    "Condition " ++ reg ++ " " ++ (operatorStr op) ++ " " ++ show val

execute :: ProgramState -> Instruction -> ProgramState
execute st Instruction { register = reg, modifyByAdding = toAdd, condition = cond } =
  if isTrue cond st
    then addToRegister toAdd reg st
    else st

newState :: ProgramState
newState = ProgramState { memory = M.empty, maxVal = 0 }

addToRegister :: Int -> Register -> ProgramState -> ProgramState
addToRegister n reg st = st { memory = mem, maxVal = mv }
  where
    mem = M.alter justAdd reg (memory st)
    mv = max (maxVal st) (M.findWithDefault 0 reg (memory st))

    justAdd Nothing  = Just n
    justAdd (Just a) = Just $ n + a

isTrue :: Condition -> ProgramState -> Bool
isTrue (Condition op reg val) st = (operatorFn op) regVal val 
  where
    regVal = M.findWithDefault 0 reg (memory st)

parseInstruction :: String -> Instruction
parseInstruction str =
  let
    [r1, cpuOp, v1, "if", r2, cmpOp, v2] = words str

    modifier "inc" = id
    modifier "dec" = negate

    parseCondOp str = Operator { operatorStr = str, operatorFn = fn }
      where
        fn = case str of
          "<"  -> (<)
          "<=" -> (<=)
          ">"  -> (>)
          ">=" -> (>=)
          "==" -> (==)
          "!=" -> (/=)
  in Instruction { register       = r1
                 , modifyByAdding = modifier cpuOp $ read v1
                 , condition      = Condition (parseCondOp cmpOp) r2 (read v2)
                 , instructionStr = cpuOp
                 }
