import Data.List.Split (splitOn)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (sort, sortOn, lookup)
import Debug.Trace
import Data.Maybe (fromJust)

main :: IO ()
main = interact (show . val . Map.fromList . zip [0..] . map parse . lines)

type Coord = (Int, Int, Int)
cz :: Coord -> Int
cz (_, _, z) = z

parse :: String -> (Coord, Coord)
parse s = case map parseCoord (splitOn "~" s) of
  [a, b] -> (a, b)

parseCoord :: String -> Coord
parseCoord s = case map read (splitOn "," s) of
  [x, y, z] -> (x, y, z)

val :: Map.Map Int (Coord, Coord) -> Int
val inAir = sum $ map (pred . Set.size) causeFall
  where
    orderedMap = Map.map sort $ Map.unionsWith (++) $ map toOrdered $ Map.toList inAir
    toOrdered (i, ((x0, y0, z0), (x1, y1, z1))) = Map.fromList [((x, y), [(z, i)]) | x <- [x0..x1], y <- [y0..y1], z <- [z0..z1] ]
    belows = Map.unionsWith (<>) $ map (belows1 . map snd) $ Map.elems orderedMap
    belows1 :: [Int] -> Map.Map Int (Set.Set Int)
    belows1 l = Map.fromList $ zipWith (\a b -> (a, Set.singleton b)) (tail l) l
    ordered = sortOn (cz . fst . snd) $ Map.toList inAir
    stacked :: Map.Map Int (Coord, Coord)
    stacked = Map.fromList stacked'
    stacked' = map (\(i, cs) -> (i, dropTo (if Map.notMember i belows then 0 else maximum (Set.map (cz . snd . (stacked Map.!)) (belows Map.! i))) cs)) ordered
    dropTo :: Int -> (Coord, Coord) -> (Coord, Coord)
    dropTo z ((x0, y0, z0), (x1, y1, z1)) = ((x0, y0, z + 1), (x1, y1, z1 - z0 + z + 1))
    supports = Map.unionsWith (<>) $ map (supports1 . map snd) $ Map.elems orderedMap
    supports1 l = Map.fromList $ zipWith (\a b -> (a, Set.fromList [b | cz (snd (stacked Map.! a)) + 1 == cz (fst (stacked Map.! b))])) l (tail l)
    a `doesSupport` b = b `elem` Map.findWithDefault mempty a supports
    supBelows = Map.mapWithKey (\a -> Set.filter (`doesSupport` a)) belows
    canDisintegrate = filter (\a -> all (\b -> Set.size (supBelows Map.! b) > 1) $ Map.findWithDefault mempty a supports) $ Map.keys inAir
    causeFall :: [Set.Set Int]
    causeFall = map (\a -> fall mempty [a]) $ Map.keys inAir
    fall :: Set.Set Int -> [Int] -> Set.Set Int
    fall fallen [] = fallen
    fall fallen0 (i:is) =
      let fallen = Set.insert i fallen0
          newFallen = Set.filter (\a -> null (supBelows Map.! a `Set.difference` fallen)) (Map.findWithDefault mempty i supports)
      in fall (fallen <> newFallen) $ is ++ Set.toList (newFallen `Set.difference` Set.fromList is)