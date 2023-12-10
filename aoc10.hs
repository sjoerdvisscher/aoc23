-- Not my initial solution, this is after some significant golfing
import Data.Array
import Data.List (find)
import Data.Maybe (fromJust)

main :: IO ()
main = interact (show . val . mkArr . lines)

mkArr :: [[Char]] -> Array (Int, Int) Char
mkArr lns' = listArray ((0, 0), (h + 2 - 1, w + 2 - 1)) (concat lns)
  where
    h = length lns'
    w = length (head lns')
    lns = [replicate (w + 2) '.'] ++ map (\l -> "." ++ l ++ ".") lns' ++ [replicate (w + 2) '.']

rs, ls, ts, bs :: [Char]
rs = "-LFS"
ls = "-J7S"
ts = "|JLS"
bs = "|F7S"

val :: Array (Int, Int) Char -> Int
val arr = part1 arr p 0
  where
    p = fst $ fromJust $ find ((== 'S') . snd) (assocs arr)

part1 :: Array (Int, Int) Char -> (Int, Int) -> Int -> Int
part1 arr p@(y, x) len = foldr
  (\(cs0, cs1, p1) next -> if c `elem` cs0 && (arr ! p1) `elem` cs1 then part1 arr' p1 (len + 1) else next)
  (part2 (elems arr') False 0) -- (len `div` 2)
  [ (rs, ls, (y, x + 1))
  , (ls, rs, (y, x - 1))
  , (bs, ts, (y + 1, x))
  , (ts, bs, (y - 1, x))
  ]
  where
    c = arr ! p
    -- Trick by Ishadijcks to only let bottom corners affect parity
    arr' = arr // [(p, if c `elem` "|LJ" then '!' else '=')]

part2 :: [Char] -> Bool -> Int -> Int
part2 [] _ c = c
part2 ('!':cs) side c = part2 cs (not side) c
part2 ('=':cs) side c = part2 cs side c
part2 (_:cs) side c = part2 cs side (c + fromEnum side)