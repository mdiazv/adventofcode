import qualified Data.Map as M
import Data.Maybe (fromMaybe, isJust)

main = do
  input <- getContents
  let rules = map parseRule $ lines input
  putStrLn . show $ part1 rules
  putStrLn . show $ part2 rules

part1 rules = sum $ map (fromMaybe 0 . penalty) $ simulate 0 rules
part2 rules = fst $ head $ dropWhile gotCaught $ allSimulations
  where
    allSimulations = [(d, map penalty $ simulate d rules) | d <- [0..]]
    gotCaught (d, ps) = any isJust ps

type Rule  = (Int, Int)
type Firewall = M.Map Int Guard
data Guard = Guard { layer :: Int
                   , range :: Int
                   , pos   :: Int
                   , dir   :: Int
                   } deriving Show
data World = World { traveler :: Int
                   , guards   :: Firewall
                   } deriving Show

delay :: Int -> World -> World
delay n w = w { guards = M.map (delayGuard n) $ guards w }
  where
    delayGuard :: Int -> Guard -> Guard
    delayGuard n g = head $ drop n' $ iterate guardStep g
      where
        n' = n `mod` (2 * (range g - 1))

world :: [Rule] -> World
world rs = World { traveler = 0
                 , guards   = M.fromList $ zip (map fst rs) (map guard rs)
                 }

guard :: Rule -> Guard
guard (depth, range) = Guard { layer = depth
                             , range = range
                             , pos   = 0
                             , dir   = 1
                             }

penalty :: World -> Maybe Int
penalty World { traveler = t, guards = gs } =
  case M.lookup t gs of
    Nothing -> Nothing
    Just g  -> if pos g == 0
                 then Just $ layer g * range g
                 else Nothing

simulate :: Int -> [Rule] -> [World]
simulate d rs = takeWhile crossing $ iterate step $ delay d w
  where
    crossing w = traveler w <= lastGuard
    (lastGuard, _) = M.findMax $ guards w
    w = world rs

step :: World -> World
step w = w { traveler = traveler w + 1
           , guards   = M.map guardStep (guards w)
           }

guardStep :: Guard -> Guard
guardStep g = g { pos = pos', dir = dir' }
  where
    pos' = pos g + dir g
    dir' = if pos' == 0 || pos' == range g - 1
              then negate $ dir g
              else dir g

parseRule :: String -> Rule
parseRule s = (read $ init depth, read range)
  where [depth, range] = words s
