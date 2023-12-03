import Data.Map (fromListWith, elems)
import Data.Monoid (Sum(..))
import Data.Char (isDigit)

main :: IO ()
main = interact (show . val . lines)

val :: [[Char]] -> Integer
val arr = getSum $ foldMap (\l -> Sum $ if length l == 2 then product l else 0) $ elems $ fromListWith (++) $
  flip foldMap [1..h] $ \y ->
    let line = arr1 !! y in
    flip foldMap [1..w] $ \x ->
      let c = line !! x
          prev = line !! (x - 1)
      in if isDigit c && not (isDigit prev) then
        let nr = takeWhile isDigit (drop x line)
            l = length nr
        in flip foldMap [-1..1] $ \dy ->
              flip foldMap [-1..l] $ \dx ->
                if arr1 !! (y + dy) !! (x + dx) == '*' then [((x + dx, y + dy), [read nr])] else []
      else []
  where
    w = length (arr !! 0)
    h = length arr
    arr1 = [replicate (w + 2) '.'] ++ map (\s -> "." ++ s ++ ".") arr ++ [replicate (w + 2) '.']
