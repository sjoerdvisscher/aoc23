{-# LANGUAGE OverloadedStrings #-}

import Data.Attoparsec.Text (Parser, decimal, sepBy1, many1, letter, parseOnly)
import Data.Map (fromList, unionsWith, elems)
import Data.String (fromString)

type Game = (Integer, [[(String, Integer)]])

main :: IO ()
main = interact (show . sum . map (val . parse) . lines)

parse :: String -> Game
parse = either error id . parseOnly gameParser . fromString

gameParser :: Parser Game
gameParser = (,) <$> ("Game " *> decimal <* ": ") <*> ((pairParser `sepBy1` ", ") `sepBy1` "; ")

pairParser :: Parser (String, Integer)
pairParser = flip (,) <$> decimal <* " " <*> many1 letter

val :: Game -> Integer
val (_, picks) =
  let vs = elems . unionsWith max . map fromList $ picks
  in if length vs < 3 then 0 else product vs

-- val :: Game -> Integer
-- val (gameId, picks) = if any (any tooBig) picks then 0 else gameId

-- tooBig :: (String, Integer) -> Bool
-- tooBig ("red", r) = r > 12
-- tooBig ("green", g) = g > 13
-- tooBig ("blue", b) = b > 14
-- tooBig _ = False
