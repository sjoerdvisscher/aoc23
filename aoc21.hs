{-# LANGUAGE TupleSections #-}

import Data.List (findIndex, elemIndex, group)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map

main :: IO ()
main = interact (show . part2 . val . lines)

val :: [[Char]] -> (Int, [Int])
val chart =
  let h = length chart
      w = length (head chart)
      sy = fromMaybe (-1) $ findIndex (elem 'S') chart
      sx = fromMaybe (-1) $ elemIndex 'S' (chart !! sy)
      go :: Map.Map (Int, Int) Int -> [(Int, Int, Int)] -> [Int]
      go _ [] = []
      go m ((_, x, y): ps) | Map.member (x, y) m || chart !! (y `mod` h) !! (x `mod` w) == '#' = go m ps
      go m ((d, x, y): ps) = d :
        go (Map.insert (x, y) d m) (ps ++ map (\(dx, dy) -> (d + 1, x + dx, y + dy)) [(0, 1), (1, 0), (0, -1), (-1, 0)])
  in (h,) $ map length $ group $ go Map.empty [(0, sx, sy)]

part2 :: (Int, [Int]) -> Int
part2 (h, res) =
  let diffs = zipWith (-) (drop h res) res
      diffs' = findCycle diffs
      findCycle as = if take h as == take h (drop h as) then cycle (take h as) else head as : findCycle (tail as)
      res' = take h res ++ zipWith (+) res' diffs'
      totals = zipWith (+) res' (0:0:totals)
  in totals !! 26501365