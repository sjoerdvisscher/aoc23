import Data.List (group, sort)

main :: IO ()
main = interact (show . sum . zipWith (*) [1..] . map snd . sort . map val . lines)

toStrength :: Char -> Int
toStrength 'A' = 14
toStrength 'K' = 13
toStrength 'Q' = 12
toStrength 'J' = 1
toStrength 'T' = 10
toStrength c = read [c]

val :: String -> (([Int], [Int]), Int)
val s = ((v2, cs), bid)
  where
    cs = map toStrength $ take 5 s
    bid = read (drop 6 s)
    v0 = sort cs
    jokers = length $ takeWhile (== 1) v0
    v1 = reverse . sort . map length . group . drop jokers $ v0
    v2 = if jokers == 5 then [5] else (head v1 + jokers) : tail v1
