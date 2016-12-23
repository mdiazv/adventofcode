module Main where

import Control.Exception (handle, SomeException)
import Data.Maybe (fromJust, isNothing)
import Data.List (isPrefixOf, partition)
import qualified Data.Map as M

main :: IO ()
main = do
  input <- getContents
  let (instructions, values) = partition ("bot" `isPrefixOf`) $ lines input
  let state = initialize (map parseValue values) $ setup $ map parse $ instructions
  let solution = simulate state
  let targetBot = target solution
  let outputList = map snd $ filter (\x -> fst x `elem` [0..2]) $ M.toList $ outputs solution
  let outputProduct = foldl (*) 1 $ outputList

  putStrLn . show $ targetBot
  putStrLn $ "(*) " ++ show outputList ++ " = " ++ show outputProduct


type BotID = Int
type OutputID = Int
type BotsState = M.Map BotID Bot
type OutputsState = M.Map OutputID Int

data State = State { bots :: BotsState
                   , outputs :: OutputsState
                   , target :: Maybe Bot
                   }
                   deriving (Show, Eq)

data Bot =
  Bot Code (Maybe Int) (Maybe Int)
  deriving (Show, Eq)

data Code =
  Code BotID Destination Destination
  deriving (Show, Eq)

data Value =
  Value BotID Int
  deriving (Show, Eq)

data Destination = ToBot BotID
                 | ToOutput OutputID
                 deriving (Show, Eq)

isTargetBot :: Int -> Int -> Bot -> Bool
isTargetBot low high (Bot _ (Just x) (Just y)) = low == (min x y) && high == (max x y)
isTargetBot _ _ _ = False

targetOne = isTargetBot 17 61

parse :: String -> Code
parse s =
    let bot      = read $ ws !! 1
        low      = read $ ws !! 6
        high     = read $ ws !! 11
        lowDest  = if ws !! 5 == "bot" then ToBot else ToOutput
        highDest = if ws !! 10 == "bot" then ToBot else ToOutput
    in  Code bot (lowDest low) (highDest high)
  where ws = words s

parseValue :: String -> Value
parseValue s =
    let n  = read $ ws !! 1
        id = read $ ws !! 5
    in  Value id n
  where ws = words s

setup :: [Code] -> State
setup code = State { bots = foldr add M.empty code, outputs = M.empty, target = Nothing }
  where
    add :: Code -> BotsState -> BotsState
    add c@(Code id _ _) = M.insert id (mkBot c)

initialize :: [Value] -> State -> State
initialize vs state = foldr init state vs
  where
    init :: Value -> State -> State
    init (Value id n) = set n id

set ::Int -> BotID -> State -> State
set n id state =
  case M.lookup id (bots state) of
    Nothing -> error $ "setting to an unexisting Bot: " ++ show id
    Just b@(Bot _ _ Nothing) -> updateBots (M.insert id (give n b)) state
    Just b -> execute b state

simulate :: State -> State
simulate s =
  let newState = run s
  in if newState == s
     then s
     else simulate newState

run :: State -> State
run state = M.foldr execute state (bots state)

execute :: Bot -> State -> State
execute bot@(Bot code@(Code id low high) (Just x) (Just y)) state =
  let
    toLow  = min x y
    toHigh = max x y
  in
    reset bot $ deliver toHigh high $ deliver toLow low $ inspectBot bot state
execute _ state = state

inspectBot :: Bot -> State -> State
inspectBot bot state =
  if targetOne bot
  then state { target = Just bot }
  else state

reset :: Bot -> State -> State
reset (Bot code@(Code id _ _) _ _) = updateBots (M.insert id (mkBot code))

deliver :: Int -> Destination -> State -> State
deliver n (ToOutput o) state = updateOutputs (output n o) state
deliver n (ToBot id)   state = updateBots (M.update (update n) id) (flush bot state)
  where
    bot :: Bot
    bot = fromJust $ M.lookup id (bots state)

    flush :: Bot -> State -> State
    flush b state
      | isFull b  = execute b state
      | otherwise = state

    update :: Int -> Bot -> Maybe Bot
    update n b = Just $ give n b

isFull :: Bot -> Bool
isFull (Bot _ _ last) = isNothing last

output :: Int -> OutputID -> OutputsState -> OutputsState
output n out = M.insert out n

give :: Int -> Bot -> Bot
give x (Bot c Nothing Nothing)   = Bot c (Just x) Nothing
give y (Bot c (Just x) Nothing)  = Bot c (Just x) (Just y)
give z bot = error $ "giving to a full bot: " ++ show bot

mkBot :: Code -> Bot
mkBot code = Bot code Nothing Nothing

updateBots :: (BotsState -> BotsState) -> State -> State
updateBots f s = s { bots = f (bots s) }

updateOutputs :: (OutputsState -> OutputsState) -> State -> State
updateOutputs f s = s { outputs = f (outputs s) }
