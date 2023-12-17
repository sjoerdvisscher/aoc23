import Data.Graph.AStar
import qualified Data.HashSet as HS

main :: IO ()
main = interact (show . val . map (map (read . pure)) . lines)

val :: [[Int]] -> Int
val layout = minimum $ map (sum . maybe [] (map cost) . aStar neighbors (const cost) heur isEnd) starts
  where
    h = length layout
    w = length (head layout)
    starts = [(0, (1, 0), (0, 0)), (0, (0, 1), (0, 0))]
    isEnd (n, _, pos) = pos == (w - 1, h - 1) && n >= 4
    heur :: (Int, (Int, Int), (Int, Int)) -> Int
    heur (_, _, (x, y)) = w - 1 - x + h - 1 - y
    cost (_, _, (x, y)) = layout !! y !! x
    neighbors (n, dir@(dx, dy), pos) = add (n + 1) dir pos $
      if n >= 4 then add 1 (dy, dx) pos $ add 1 (-dy, -dx) pos mempty else mempty
    add n dir@(dx, dy) (x, y) q = if x' < 0 || y' < 0 || x' >= w || y' >= w || n > 10 then q else HS.insert (n, dir, (x', y')) q
      where
        x' = x + dx
        y' = y + dy
