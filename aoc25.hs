import Data.List.Split (splitOn)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Math.Clustering.Spectral.Sparse (spectralClusterKNorm)
import Data.Sparse.SpMatrix (mkSpMR)
import Data.Sparse.SpVector (toListSV)
import Data.List (nub)

main :: IO ()
main = interact (show . val . map parse . lines)

parse :: String -> (String, [String])
parse s = (take 3 s, splitOn " " (drop 5 s))

val :: [(String, [String])] -> Int
val adj = if length cuts == 3 then Set.size cluster * (n - Set.size cluster) else 0
  where
    connections = concatMap (\(a, bs) -> map (\b -> (a, b)) bs) adj
    names = nub $ map fst adj ++ concatMap snd adj
    ix = Map.fromList $ zip names [0..]
    nm = Map.fromList $ zip [0..] names
    n = Map.size ix
    adjMat = mkSpMR (n, n) $ concatMap (\(a, b) -> let ai = ix Map.! a; bi = ix Map.! b in [(ai, bi, 1), (bi, ai, 1)]) connections
    cluster = Set.fromList $ map ((nm Map.!) . fst) $ toListSV $ spectralClusterKNorm 1 2 adjMat
    cuts = filter (\(a, b) -> Set.member a cluster /= Set.member b cluster) connections
