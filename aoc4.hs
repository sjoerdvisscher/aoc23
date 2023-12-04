{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

import Data.Attoparsec.Text (Parser, parseOnly, decimal, sepBy1)
import Data.String (fromString)
import Data.Monoid (Sum(..))
import Control.Applicative (many)

main :: IO ()
main = interact (show . val . map parse . lines)

type Card = ([Int], [Int])

parse :: String -> Card
parse = either error id . parseOnly cardParser . fromString

decimal' :: Parser Int
decimal' = many " " *> decimal

cardParser :: Parser Card
cardParser = (,) <$> (("Card " *> decimal' <* ": ") *> (decimal' `sepBy1` " ")) <*> (" | " *> (decimal' `sepBy1` " "))

count :: Card -> Int
count (winning, have) = getSum $ foldMap (\n -> Sum $ if n `elem` winning then 1 else 0) have

val :: [Card] -> Int
val = go 0 . map (, 1)
  where
    go :: Int -> [(Card, Int)] -> Int
    go total [] = total
    go total ((card, amount) : cards) =
      let c = count card
          extra = replicate c amount ++ repeat 0
          cards' = zipWith (\(crd, nr) e -> (crd, nr + e)) cards extra
      in go (total + amount) cards'
