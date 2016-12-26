import Control.Monad (filterM)
import Control.Monad.State (State, evalState, get, modify)
import Data.Digest.Pure.MD5 (md5)
import Data.List (isInfixOf)
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.Map.Lazy as M

type Key = (Int, String)
type Hash = String
type Salt = String
type HashFn = Salt -> Int -> Hash
type KeyMap = M.Map Int String

data HashCache = HashCache { hashfn :: HashFn
                           , salt   :: Salt
                           , cache  :: KeyMap
                           }

main :: IO ()
main = do
  let testSalt = "abc"
  let realSalt = "ahsbgdzn"
  putStrLn "Test - regular hash"
  solve hash testSalt
  putStrLn "Real - regular hash"
  solve hash realSalt
  putStrLn "Test - stretch 2016"
  solve (stretch 2016) testSalt
  putStrLn "Real - stretch 2016"
  solve (stretch 2016) realSalt

cutoff :: Int
cutoff = 30000

solve :: HashFn -> Salt -> IO ()
solve hash salt = do
  putStrLn . show . last $ take 64 $ cachedHash hash salt [1..cutoff]
--  putStrLn . unlines $ map show $ take 64 $ cachedHash hash salt [1..cutoff]

plainMD5 :: String -> String
plainMD5 = show . md5 . C8.pack

hash :: HashFn
hash salt i = plainMD5 $ salt ++ show i

stretch :: Int -> HashFn
stretch n salt i = foldr (.) id (replicate n plainMD5) $ hash salt i

cachedHash :: HashFn -> Salt -> [Int] -> [Key]
cachedHash hash salt ns = evalState (getKeys ns) (emptyCache hash salt)

getKeys :: [Int] -> State HashCache [Key]
getKeys ns = do
  ks <- hashLots ns
  filterM isKey ks

hashLots :: [Int] -> State HashCache [Key]
hashLots [] = return []
hashLots (n:ns) = do
  k <- hashIt n
  ks <- hashLots ns
  return (k:ks)

hashIt :: Int -> State HashCache Key
hashIt n = do
  st <- get
  let cached = M.lookup n (cache st)
  case cached of
    Just h  -> return (n, h)
    Nothing -> do
      let hash = (hashfn st) (salt st) n
      modify $ (modifyCache (M.insert n hash))
      return (n, hash)
  where
    modifyCache :: (KeyMap -> KeyMap) -> HashCache -> HashCache
    modifyCache f hc = hc { cache = f (cache hc) }

isKey :: Key -> State HashCache Bool
isKey (x, s) =
  case getTriplet s of
    Nothing -> return False
    Just c  -> do
      hashes <- hashLots [x+1..x+1000]
      return $ any (contains (replicate 5 c)) hashes
  where
    contains :: String -> Key -> Bool
    contains c5 (_, s) = c5 `isInfixOf` s

getTriplet :: String -> Maybe Char
getTriplet s =
  let triplet = take 1 $ filter allSame $ zip3 s (tail s) (tail $ tail s)
  in case triplet of
    []        -> Nothing
    [(c,_,_)] -> Just c
  where
    allSame :: (Char, Char, Char) -> Bool
    allSame (a,b,c) = a == b && b == c

emptyCache :: HashFn -> Salt -> HashCache
emptyCache hash salt = HashCache { hashfn = hash, salt = salt, cache = M.empty }
