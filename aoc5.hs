{-# LANGUAGE OverloadedStrings #-}

import Data.Attoparsec.Text (Parser, parseOnly, decimal, sepBy1, space, endOfLine, takeWhile, endOfInput)
import Data.String (fromString)
import Control.Applicative (many)
import Control.Monad (void)
import Prelude hiding (takeWhile)

type R = (Int, Int)

main :: IO ()
main = interact (show . val . parse)

type Almanac = ([R], [[(Int, Int, Int)]])

parse :: String -> Almanac
parse = either error id . parseOnly almanacParser . fromString

almanacParser :: Parser Almanac
almanacParser = (,) <$> seedsParser <*> many mapParser <* endOfInput

seedsParser :: Parser [R]
seedsParser = "seeds: " *> (((,) <$> decimal <* space <*> decimal) `sepBy1` space) <* endOfLine

mapParser :: Parser [(Int, Int, Int)]
mapParser = do
  void endOfLine
  void $ takeWhile (/= '\n')
  void endOfLine
  many ((,,) <$> decimal <* space <*> decimal <* space <*> decimal <* endOfLine)

val :: Almanac -> Int
val (seeds, mapDefs) = minimum $ map fst $ seeds `mapping` mapDefs

mapping :: [R] -> [[(Int, Int, Int)]] -> [R]
mapping = foldl mappingN

mappingN :: [R] -> [(Int, Int, Int)] -> [R]
mappingN rs [] = rs
mappingN rs (m:ms) = let (unmapped, mapped) = unzip (map (`mapping1` m) rs) in
  concat mapped ++ mappingN (concat unmapped) ms

mapping1 :: R -> (Int, Int, Int) -> ([R], [R])
mapping1 (lo, w) (ds, ss, l) =
  let hi = lo + w; se = ss + l in
    if hi <= ss then ([(lo, w)], []) else
      if hi <= se then
        if lo < ss then ([(lo, ss - lo)], [(ds, hi - ss)])
        else ([], [(lo - ss + ds, w)])
      else if lo < ss then ([(lo, ss - lo), (se, hi - se)], [(ds, l)])
        else if lo < se then ([(se, hi - se)], [(lo - ss + ds, se - lo)])
          else ([(lo, w)], [])
