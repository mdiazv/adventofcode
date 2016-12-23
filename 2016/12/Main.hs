module Main where

import Control.Applicative
import Data.List (isPrefixOf)
import Data.Maybe (fromJust)
import qualified Data.Map as M

main :: IO ()
main = do
  input <- getContents
  let load = computer . compile . lines
  putStrLn . show $ getRegister 'a' $ run $ load input
  putStrLn . show $ getRegister 'a' $ run $ setRegister 1 'c' $ load input

type RegisterID = Char
data Value = FromRegister RegisterID
           | Value Int
           deriving (Show, Eq)
data Instruction = Copy Value RegisterID
                 | Inc RegisterID
                 | Dec RegisterID
                 | Jump Value Int
                 deriving (Show, Eq)
type Code = ([Instruction], [Instruction])
data Computer = Computer { code      :: Code
                         , registers :: M.Map RegisterID Int
                         }
                         deriving (Show, Eq)

compile :: [String] -> Code
compile ss = ([], map compile' ss)
  where
    compile' :: String -> Instruction
    compile' s
      | "cpy" `isPrefixOf` s = Copy valueX (toRegisterID y)
      | "inc" `isPrefixOf` s = Inc regX
      | "dec" `isPrefixOf` s = Dec regX
      | "jnz" `isPrefixOf` s = Jump valueX (read y)
      where
        ws = words s
        cmd = ws !! 0
        x = ws !! 1
        y = ws !! 2
        regX = toRegisterID x
        valueX = if regX `elem` ['0'..'9']
                 then Value (read x)
                 else FromRegister regX

run :: Computer -> Computer
run c@(Computer { code = code, registers = registes }) =
  case instructionFetch code of
    Nothing -> c
    Just i  -> run $ execute i c

execute :: Instruction -> Computer -> Computer
execute i c =
  case i of
    Copy v r -> advance $ cpy v r c
    Inc x    -> advance $ inc x c
    Dec x    -> advance $ dec x c
    Jump x y -> jnz x y c
  where advance = moveInstructionPointer 1

cpy :: Value -> RegisterID -> Computer -> Computer
cpy (Value n)        r c = setRegister n r c
cpy (FromRegister n) r c = setRegister (getRegister n c) r c

inc, dec :: RegisterID -> Computer -> Computer
inc = updateRegister succ
dec = updateRegister pred

jnz :: Value -> Int -> Computer -> Computer
jnz r n c = moveInstructionPointer amount c
  where
    amount = if value /= 0 then n else 1
    value = case r of
      FromRegister n -> getRegister n c
      Value n        -> n

moveInstructionPointer :: Int -> Computer -> Computer
moveInstructionPointer n c = c { code = advance n (code c) }
  where
    advance :: Int -> Code -> Code
    advance n c
      | n == 0 = c
      | n >  0 = advance (n-1) $ forward  c
      | n <  0 = advance (n+1) $ backward c
      where
        forward, backward :: Code -> Code
        forward  c@(_, []) = c
        forward  (left, i:right) = (i:left, right)
        backward c@([], _) = c
        backward (i:left, right) = (left, i:right)

instructionFetch :: Code -> Maybe Instruction
instructionFetch (_, []) = Nothing
instructionFetch (_, (i:_)) = Just i

setRegister :: Int -> RegisterID -> Computer -> Computer
setRegister n r c = c { registers = M.insert r n (registers c) }

updateRegister :: (Int -> Int) -> RegisterID -> Computer -> Computer
updateRegister f r c = c { registers = M.update (\n -> Just $ f n) r (registers c) }

getRegister :: RegisterID -> Computer -> Int
getRegister r c = fromJust $ M.lookup r $ registers c

computer :: Code -> Computer
computer c = Computer { code = c, registers = M.fromList [(r, 0) | r <- "abcd"] }

toRegisterID :: String -> RegisterID
toRegisterID = head
