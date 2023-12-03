import Data.Bifunctor (first)
import Data.Monoid (First(..))
import Data.List (tails, isPrefixOf)
import Data.Maybe (fromMaybe)

main :: IO ()
main = interact (show . sum . map val . lines)

val :: String -> Integer
val s = findFirst digits s * 10 + findFirst (map (first reverse) digits) (reverse s)

findFirst :: [(String, Integer)] -> String -> Integer
findFirst ds = fromMaybe 0 . getFirst . foldMap (startsAny ds) . tails

startsAny :: [(String, a)] -> String -> First a
startsAny ds s = foldMap (\(d, a) -> First $ if d `isPrefixOf` s then Just a else Nothing) ds

digits :: [(String, Integer)]
digits =
  [ ("1", 1), ("2", 2), ("3", 3), ("4", 4), ("5", 5)
  , ("6", 6), ("7", 7), ("8", 8), ("9", 9)
  , ("one", 1), ("two", 2), ("three", 3), ("four", 4), ("five", 5)
  , ("six", 6), ("seven", 7), ("eight", 8), ("nine", 9)
  ]