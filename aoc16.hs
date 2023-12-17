import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

main :: IO ()
main = interact (show . val . lines)

type Pos = (Int, Int)
type Dir = (Int, Int)
type Energized = Map.Map Pos [Dir]

val :: [[Char]] -> Int
val layout = maximum $ map (Map.size . uncurry (go Map.empty)) edges
  where
    go :: Energized -> Pos -> Dir -> Energized
    go m p d | d `elem` fromMaybe [] (Map.lookup p m) = m
    go m p@(x, y) d@(dx, dy) = let m' = Map.unionWith (++) m (Map.singleton p [d]) in case layout !! y !! x of
      '.' -> step m' p d
      '\\' -> step m' p (dy, dx)
      '/' -> step m' p (-dy, -dx)
      '-' -> if dy == 0 then step m' p d else step (step m' p (-1, 0)) p (1, 0)
      '|' -> if dx == 0 then step m' p d else step (step m' p (0, -1)) p (0, 1)
      c -> error [c]
    step m (x, y) (dx, dy) =
      let x' = x + dx; y' = y + dy
      in if x' < 0 || y' < 0 || x' > w || y' > h then m
          else go m (x', y') (dx, dy)
    h = length layout - 1
    w = length (head layout) - 1
    edges =
      [((x, 0), (0, 1)) | x <- [0 .. w]]
      ++ [((x, h), (0, -1)) | x <- [0 .. w]]
      ++ [((0, y), (1, 0)) | y <- [0 .. h]]
      ++ [((w, y), (-1, 0)) | y <- [0 .. h]]