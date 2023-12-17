import Data.List.Split (splitOn)
import Data.List (foldl', findIndex)
import Debug.Trace (traceShowId)

main :: IO ()
main = interact (show . part2 . splitOn ",")

hash :: String -> Int
hash = foldl' (\h c -> ((fromEnum c + h) * 17) `mod` 256) 0 . traceShowId

part2 :: [String] -> Int
part2 = val . foldl' f (replicate 256 [])
  where
    f bs act =
      let (label, rest) = break (\c -> c == '-' || c == '=') act
          bi = hash label
          box = bs !! bi
          idx = findIndex ((== label) . fst) box
          box' = case head rest of
            '-' -> maybe box (\i -> take i box ++ drop (i + 1) box) idx
            _ -> let l = read (tail rest) in maybe (box ++ [(label, l)]) (\i -> take i box ++ [(label, l)] ++ drop (i + 1) box) idx
      in take bi bs ++ [box'] ++ drop (bi + 1) bs

val :: [[(String, Int)]] -> Int
val = sum . zipWith (\bi -> sum . zipWith (\li (_, l) -> bi * li * l) [1..]) [1..]