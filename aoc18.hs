import qualified Data.Map as Map
import Data.List (sortOn)
import Data.List.Split (splitOn)

main :: IO ()
main = interact (show . val . map parse2 . lines)

parse1 :: String -> (Char, Int)
parse1 s = case splitOn " " s of
  [a, b, _] -> (head a, read b)
  _ -> error s

parse2 :: String -> (Char, Int)
parse2 s = case splitOn " " s of
  [_, _, c] -> (conv (c !! 7), read ("0x" ++ take 5 (drop 2 c)))
  _ -> error s

conv :: Char -> Char
conv '0' = 'R'
conv '1' = 'D'
conv '2' = 'L'
conv _ = 'U'

val :: [(Char, Int)] -> Int
val = go Map.empty 0 0
  where
    go :: Map.Map Int [(Int, Char)] -> Int -> Int -> [(Char, Int)] -> Int
    go m _ _ [] = fill m
    go m x y (('R', n):is) = go (add m [(y, [(x, 'r'), (x + n, 'r')])]) (x + n) y is
    go m x y (('L', n):is) = go (add m [(y, [(x - n, 'l'), (x, 'l')])]) (x - n) y is
    go m x y (('U', n):is) = go (add m [(y', [(x, 'U')])|y' <- [y - n .. y]]) x (y - n) is
    go m x y ((_, n):is) = go (add m [(y', [(x, 'D')])|y' <- [y .. y + n]]) x (y + n) is
    add m coords = Map.unionWith (++) m (Map.fromList coords)
    fill = sum . map (fillLine . mergeOutside . merge . sortOn fst) . Map.elems
    merge (a:b:cs) | fst a == fst b = (fst a, [snd a, snd b]) : merge cs
    merge (a:cs) = (fst a, [snd a]) : merge cs
    merge [] = []
    mergeOutside (a:b:cs)
      | isPair (snd a) (snd b) = a : mergeInside cs
    mergeOutside (a:cs) = a : mergeInside cs
    mergeOutside [] = []
    isPair [a, b] [c, d]
      | a == d && b == c = True
      | a == c && b == d = True
    isPair _ _ = False
    mergeInside (a:b:cs)
      | isPair (snd a) (snd b) = b : mergeOutside cs
      | snd a == "lD" && snd b == "Ul" = mergeInside cs
      | snd a == "lU" && snd b == "Dl" = mergeInside cs
      | snd a == "Dr" && snd b == "rU" = mergeInside cs
      | snd a == "Ur" && snd b == "rD" = mergeInside cs
    mergeInside (a:cs) = a : mergeOutside cs
    mergeInside [] = []
    fillLine [] = 0
    fillLine [_] = error "uneven length"
    fillLine cs | length cs `mod` 2 == 1 = error $ show cs
    fillLine (a:b:xs) = fst b - fst a + 1 + fillLine xs
