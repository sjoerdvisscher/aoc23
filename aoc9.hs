main :: IO ()
main = interact (show . sum . map (val . map read . words) . lines)

val :: [Int] -> Int
val xs | all (== 0) xs = 0
val xs = head xs - val (zipWith (-) (tail xs) (init xs))