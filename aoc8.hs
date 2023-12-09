import qualified Data.Map as Map
import Data.List (sort)

main :: IO ()
main = interact (val . parse . lines)

parse :: [String] -> (String, Map.Map String (String, String))
parse (rl:_:ls) = (rl, Map.fromList $ map parseLine ls)

parseLine :: String -> (String, (String, String))
parseLine s = (take 3 s, (take 3 (drop 7 s), take 3 (drop 12 s)))

val :: (String, Map.Map String (String, String)) -> String
val (rl, m) = if start == loops then show $ foldr (lcm . snd) 1 loops else "start and loops doesn't match :("
  where
    as = filter (\(_:_:a) -> a == "A") $ Map.keys m
    zs = filter (\(_:_:a) -> a == "Z") $ Map.keys m
    start = sort $ map (go (cycle rl) 0) as
    loops = sort $ map (go (cycle rl) 0) zs
    go :: String -> Int -> String -> (String, Int)
    go _ i p@(_:_:"Z") | i > 0 = (p, i)
    go (rl:rls) i pos = go rls (i + 1) $ let (l, r) = m Map.! pos in if rl == 'L' then l else r
