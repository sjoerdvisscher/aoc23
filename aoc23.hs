{-# LANGUAGE TupleSections #-}
import qualified Data.Map as Map

main :: IO ()
main = interact (show . val . lines)

type Coord = (Int, Int)
val :: [[Char]] -> Int
val layout = maximum (findPaths [(1,0)] (1,0))
  where
    h = length layout - 1
    follow :: Int -> Coord -> Coord -> ([Coord], Coord, Int)
    follow len prev (x, y)
      | y == h = ([], (x, y), len)
      | otherwise = if length opts == 1
        then follow (len + 1) (x, y) (head opts)
        else (filter check opts, (x, y), len)
      where
        opts = filter (\(x,y) -> prev /= (x, y) && layout !! y !! x /= '#')
             . map (\(dx, dy) -> (x + dx, y + dy))
             $ [(1, 0), (0, 1), (-1, 0), (0, -1)]
        check (x', y') = case (x' - x, y' - y, layout !! y' !! x') of
          (1, 0, '>') -> True
          (-1, 0, '<') -> True
          (0, 1, 'v') -> True
          (0, -1, '^') -> True
          _ -> False

    go :: Map.Map (Coord, Coord) (Coord, Int) -> [(Coord, Coord)] -> Map.Map (Coord, Coord) (Coord, Int)
    go acc [] = acc
    go acc ((p0, next):xs) = case follow 1 p0 next of
      (opts, p1, l) -> let acc' = Map.insert (p0, next) (p1, l) acc in
        go acc' (xs ++ filter (`Map.notMember` acc') (map (p1, ) opts))

    steps1, steps :: Map.Map Coord [(Coord, Int)]
    steps1 = Map.mapKeysWith (++) fst $ Map.map pure $ go Map.empty [((1, 0), (1, 1))]
    steps = Map.unionWith (++) steps1 (invertMap steps1)
    invertMap = Map.fromListWith (++) . concatMap (\(p0, pls) -> map (\(p1, l) -> (p1, [(p0, l)])) pls) . Map.toList

    findPaths :: [Coord] -> Coord -> [Int]
    findPaths visited p
      | snd p == h = [0]
      | otherwise = foldMap (\(p1, l) -> if p1 `elem` visited then [] else map (+l) $ findPaths (p:visited) p1) (steps Map.! p)