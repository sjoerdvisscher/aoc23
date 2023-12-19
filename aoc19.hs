{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

import Data.Attoparsec.Text (Parser, parseOnly, many1, char, letter, sepBy1, decimal, anyChar, endOfLine)
import qualified Data.Map as Map
import Control.Monad (void)
import Data.String (fromString)

main :: IO ()
main = interact (show . part2 . parse)

type Rule = ([(Char, Char, Int, String)], String)
type Part = Map.Map Char Int

parse :: String -> (Map.Map String Rule, [Part])
parse = either error id . parseOnly ((,) <$> (Map.fromList <$> many1 ruleParser) <* endOfLine <*> many1 partParser) . fromString

ruleParser :: Parser (String, Rule)
ruleParser = do
  nm <- many1 letter
  void (char '{')
  tests <- ((,,,) <$> letter <*> anyChar <*> decimal <* char ':' <*> many1 letter) `sepBy1` char ','
  void (char ',')
  final <- many1 letter
  void (char '}')
  void endOfLine
  pure (nm, (tests, final))

partParser :: Parser Part
partParser = do
  void (char '{')
  props <- ((,) <$> letter <* char '=' <*> decimal) `sepBy1` char ','
  void (char '}')
  void endOfLine
  pure $ Map.fromList props

part1 :: (Map.Map String Rule, [Part]) -> Int
part1 (rules, parts) = sum $ map (go "in") parts
  where
    go "A" part = sum (Map.elems part)
    go "R" _ = 0
    go lbl part =
      let (tests, final) = rules Map.! lbl
          check (p, '<', val, a) b = if part Map.! p < val then a else b
          check (p, _, val, a) b = if part Map.! p > val then a else b
      in go (foldr check final tests) part

part2 :: (Map.Map String Rule, [Part]) -> Int
part2 (rules, _) = go "in" . Map.fromList . fmap (, [1..4000]) $ "xmas"
  where
    go "A" parts = product . map length . Map.elems $ parts
    go "R" _ = 0
    go lbl parts =
      let (tests, final) = rules Map.! lbl
          go2 [] ps = go final ps
          go2 ((p, op, val, a):ts) ps = go2 ts (Map.adjust (filter (if op == '<' then (>= val) else (<= val))) p ps)
            + go a (Map.adjust (filter (if op == '<' then (< val) else (> val))) p ps)
      in go2 tests parts
