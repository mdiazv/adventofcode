import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (fromJust)
import Data.Tuple (swap)

main = do
  input <- getContents
  let cs = map parseConn $ lines input :: [Neighbors]
  putStrLn . show $ part1 cs
  putStrLn . show $ part2 cs

part1 = S.size . (reachable 0) . toVillage
part2 = length . components . toVillage

type Component = S.Set Int
type Neighbors = (Int, [Int])
type Village = M.Map Int (S.Set Int)

components :: Village -> [Component]
components v = fst $ foldl findComponent ([], v) $ M.keys v
  where
    findComponent (cs, v) k =
      if k `M.member` v
        then (c:cs, v')
        else (cs, v)
      where
        c  = reachable k v
        v' = S.foldl (flip M.delete) v c

reachable :: Int -> Village -> S.Set Int
reachable n v = dfs S.empty n
  where
    dfs :: S.Set Int -> Int -> S.Set Int
    dfs closed n = if n `S.member` closed
                     then closed
                     else foldl dfs closed' $ neighbors n
      where closed' = S.insert n closed

    neighbors n = fromJust $ M.lookup n v

toVillage :: [Neighbors] -> Village
toVillage nbs = foldl addAll initial nbs
  where
    initial = M.fromList $ zip (map fst nbs) (repeat S.empty)

    addAll :: Village -> Neighbors -> Village
    addAll v (k, nb) = foldl addToBoth v $ zip (repeat k) nb
      where
        addToBoth v (a, b) = add a b $ add b a v
        add a b = M.update (Just . (S.insert b)) a

parseConn :: String -> Neighbors
parseConn s = (read from, toList nbs)
  where
    (from : sep : nbs) = words s
    toList str = (map (read . init) $ init str) ++ [read $ last str]
