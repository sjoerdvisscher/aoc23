{-# LANGUAGE TupleSections #-}
import qualified Data.Map as Map
import Data.List.Split (splitOn)
import Data.Bifunctor (bimap)
import Data.Containers.ListUtils (nubOrdOn)

main :: IO ()
main = interact (show . val . map parse . lines)

newtype Machine = M { run :: (String, Bool) -> (Machine, Maybe Bool) }

broadcaster :: Machine
broadcaster = M $ \(_, b) -> (broadcaster, Just b)

flipFlop :: Machine
flipFlop = go False
  where
    go mem = M $ \(_, b) -> if b then (go mem, Nothing) else (go (not mem), Just (not mem))

conj :: Int -> Machine
conj size = go Map.empty
  where
    go mem = M $ \(nm, b) ->
      let mem' = Map.insert nm b mem
      in (go mem', Just $ not (Map.size mem' == size && and (Map.elems mem')))

parse :: String -> (String, [String])
parse s = (nm, outputs)
  where
    [nm, outs] = splitOn " -> " s
    outputs = splitOn ", " outs

val :: [(String, [String])] -> Int
val inps = foldr (lcm . snd) 1 $ take (length targetInps) $ nubOrdOn fst $ go 0 machines0 (0, 0) []
  where
    (machines0, outputs) = bimap Map.fromList Map.fromList $ unzip $ map mkMachine inps
    mkMachine (nm, outs) = case nm of
      ('%':nm') -> ((nm', flipFlop), (nm', outs))
      ('&':nm') -> ((nm', conj (length (findInputs nm'))), (nm', outs))
      _ -> ((nm, broadcaster), (nm, outs))
    target = head (findInputs "rx")
    targetInps = findInputs target
    findInputs tgt = map (tail . fst) . filter (elem tgt . snd) $ inps

    go n machines lh [] = go (n + 1) machines lh [("button", False, "broadcaster")]
    go n machines (los, his) ((from, b, to):msgs) =
        (if to == target && b then ((from, n):) else id)
        $ go n machines' (los + fromEnum (not b), his + fromEnum b) (msgs ++ msgs')
      where
        (machines', msgs') = maybe
          (machines, [])
          (bimap (\m -> Map.insert to m machines) (maybe [] (\b' -> map (to, b',) (outputs Map.! to))) . (`run` (from, b)))
          (Map.lookup to machines)
