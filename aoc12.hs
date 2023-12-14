import Data.String (fromString)
import Data.Attoparsec.Text (parseOnly, decimal, space, takeWhile, sepBy1, char)
import Prelude hiding (takeWhile)
import Data.Function.Memoize (memoize2)
import Data.Text (unpack)
import Data.Word (Word8)

main :: IO ()
main = interact (show . sum . map (val . parse) . lines)

parse :: String -> ([Char], [Word8])
parse = either error id . parseOnly ((,) <$> (unpack <$> takeWhile (/= ' ')) <* space <*> (decimal `sepBy1` char ',')) . fromString

val :: ([Char], [Word8]) -> Int
val (chars, counts) = go (map mapChars $ tail (concat (replicate 5 ('?':chars))) ++ ".") (concat (replicate 5 counts))
  where
    mapChars '.' = Just False
    mapChars '#' = Just True
    mapChars _ = Nothing
    go = memoize2 go'
    go' :: [Maybe Bool] -> [Word8] -> Int
    go' [] gs = if null gs then 1 else 0
    go' cs [] = if Just True `elem` cs then 0 else 1
    go' cs (g':gs) = let g = fromIntegral g' in
      (if head cs /= Just True then go (tail cs) (g':gs) else 0)
      + (if Just False `elem` take g cs then 0 else if length cs == g || cs !! g /= Just True then go (drop (g + 1) cs) gs else 0)
