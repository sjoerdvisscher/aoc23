import Data.List (transpose)

main :: IO ()
main = interact (show . val . lines)

val :: [[Char]] -> Int
val chart = part1 chart + part1 (transpose chart)

part1 :: [[Char]] -> Int
part1 chart = count 0 0 galaxies
  where
    galaxies = map (length . filter (=='#')) chart
    total = sum galaxies
    count _ acc [] = acc
    count seen acc (0:gs) = count seen (acc + 1000000 * seen * (total - seen)) gs
    count seen acc (g:gs) = count (seen + g) (acc + seen * (total - seen)) gs
