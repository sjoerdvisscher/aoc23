{-# LANGUAGE OverloadedStrings #-}

import Data.Attoparsec.Text (Parser, parseOnly, decimal, skipSpace, endOfLine, takeWhile)
import Data.String (fromString)
import Control.Applicative (many)
import Control.Monad (void)
import Prelude hiding (takeWhile)

main :: IO ()
main = interact (show . product . map val . uncurry zip . parse)

parse :: String -> ([Int], [Int])
parse = either error id . parseOnly ((,) <$> partParser <*> partParser) . fromString

partParser :: Parser [Int]
partParser = do
  void $ takeWhile (/= ' ')
  res <- many (skipSpace *> decimal)
  endOfLine
  pure res

-- x * x - t * x + d = 0
-- x = t/2 +/- (t*t/4 - d)
val :: (Int, Int) -> Int
val (t, d) = floor maxT - ceiling minT + 1
  where
    disc :: Double
    disc = sqrt (fromIntegral t * fromIntegral t / 4 - fromIntegral d - 0.01)
    maxT = fromIntegral t / 2 + disc
    minT = fromIntegral t / 2 - disc

