module Main where

main :: IO ()
main = do
  input <- getContents
  putStrLn . show . length $ filter (supports xTLS) $ lines input
  putStrLn . show . length $ filter (supports xSSL) $ lines input

data State = TLSState { inBrackets :: Bool
                      , abbaFound  :: Bool
                      }
           | SSLState { inBrackets :: Bool
                      , abas       :: [String]
                      , babs       :: [String]
                      }

xTLS :: State
xTLS = TLSState { inBrackets = False, abbaFound = False }

xSSL :: State
xSSL = SSLState { inBrackets = False, abas = [], babs = []}

supports :: State -> String -> Bool
supports st s = 
  case st of
    -- There should be a better way to do this Pattern Match
    TLSState _ _ -> searchTLS st s
    SSLState _ _ _ -> searchSSL st s

searchTLS :: State -> String -> Bool
searchTLS st [] = abbaFound st
searchTLS st s@(c:cs) =
  case c of
    '[' -> searchTLS (st { inBrackets = True  }) cs
    ']' -> searchTLS (st { inBrackets = False }) cs
    _   -> if isABBA s
           then case inBrackets st of
             True  -> False
             False -> searchTLS (st { abbaFound = True }) cs
           else searchTLS st cs

searchSSL :: State -> String -> Bool
searchSSL st [] = match (abas st) (babs st)
searchSSL st s@(c:cs) =
  -- This code is way too similar to searchTLS
  -- Maybe with type classes I can get around with only one definition
  case c of
    '[' -> searchSSL (st { inBrackets = True  }) cs
    ']' -> searchSSL (st { inBrackets = False }) cs
    _   -> if isABA s
           then case inBrackets st of
             True  -> searchSSL (st { babs = [c2,c]:(babs st) }) cs
             False -> searchSSL (st { abas = [c,c2]:(abas st) }) cs
           else searchSSL st cs
  where c2 = head cs

isABBA :: String -> Bool
isABBA (a:b:c:d:xs) = a == d && b == c && a /= b
isABBA _ = False

isABA :: String -> Bool
isABA (a:b:c:xs) = a == c && a /= b
isABA _ = False

match :: [String] -> [String] -> Bool
match abas babs = any (`elem` babs) abas
