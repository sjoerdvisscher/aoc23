import Data.List

main :: IO ()
main = interact (show . part2 [] . transpose . lines)

moveWest :: [Char] -> [Char]
moveWest [] = []
moveWest ('#':cs) = '#':moveWest cs
moveWest cs =
  let (inp, rest) = break (== '#') $ cs
  in reverse (sort inp) ++ moveWest rest

loadWest :: [Char] -> Int
loadWest cs = go (length cs) cs
  where
    go _ [] = 0
    go l ('O':cs) = l + go (l - 1) cs
    go l (_:cs) = go (l - 1) cs

part2 :: [[[Char]]] -> [[Char]] -> Int
part2 history chart =
  let chart' = iterate (reverse . transpose . map moveWest) chart !! 4
  in case elemIndex chart' history of
    Nothing -> part2 (history ++ [chart']) chart'
    Just i ->
      let cycleLen = length history - i
          idx = ((1000000000 - 1 - i) `mod` cycleLen) + i
      in sum . map loadWest $ history !! idx
