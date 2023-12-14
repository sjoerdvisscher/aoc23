import Data.List.Split (splitOn)
import Data.List (transpose)

main :: IO ()
main = interact (show . (`div` 2) . sum . map (val . splitOn "\n") . splitOn "\n\n")

val :: [[Char]] -> Int
val chart = let p1r = part1 0 chart; p1c = part1 0 (transpose chart) in
  sum $ concat $
    flip map [0..length chart - 1] \y ->
      flip map [0..length (head chart) - 1] \x ->
        let chart' = update x y chart in
          100 * part1 p1r chart' + part1 p1c (transpose chart')

update :: Int -> Int -> [[Char]] -> [[Char]]
update x y chart = take y chart ++ [line'] ++ drop (y+1) chart
  where
    line = chart !! y
    line' = take x line ++ [char'] ++ drop (x+1) line
    char = line !! x
    char' = if char == '.' then '#' else '.'

part1 :: Int -> [String] -> Int
part1 r' as = go 1 (l - 2)
  where
    bs = reverse as
    l = length as
    go r n
      | r == l = 0
      | and (zipWith (==) (drop (-n) as) (drop n bs)) && r /= r' = r
      | otherwise = go (r + 1) (n - 2)
