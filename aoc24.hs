import Data.List.Split (splitOn)

main :: IO ()
main = interact (show . part2 . map parse . lines)

type Coord = (Integer, Integer, Integer)

parse :: String -> (Coord, Coord)
parse s = case map parseCoord (splitOn "@" s) of
  [a, b] -> (a, b)

parseCoord :: String -> Coord
parseCoord s = case map read (splitOn "," s) of
  [x, y, z] -> (x, y, z)

hit :: (Coord, Coord) -> (Coord, Coord) -> (Integer, Integer, Coord)
hit ((x1, y1, z1), (vx1, vy1, vz1)) ((x2, y2, z2), (vx2, vy2, vz2)) = (d, vz, (x, y, z))
  where
    t2 = (y2 - y1)*vx1 - (x2 - x1)*vy1
    t1 = (y1 - y2)*vx2 - (x1 - x2)*vy2
    d = vy1*vx2 - vx1*vy2
    vz = ((z2 - z1)*d + t2*vz2 + t1*vz1) `div` (t1 + t2)
    z = z1 - t1*(vz1 - vz) `div` d
    x = x1 - t1*vx1 `div` d
    y = y1 - t1*vy1 `div` d

part2 :: [(Coord, Coord)] -> Integer
part2 ((p1, v1):(p2, v2):(p3, v3):_) = ax + ay + az
  where
    velocities = [ v | s <- [1..], t <- [-s..s], v <- [(s, t, 0), (-s, t, 0), (t, s, 0), (t, -s, 0)]]
    (ax, ay, az) = foldr (\v@(vx, vy, _) next ->
      let (d12, vz, p12) = hit (p1, subCoord v1 v) (p2, subCoord v2 v)
          v' = (vx, vy, vz)
          (d13, vz', p13) = hit (p1, subCoord v1 v') (p3, subCoord v3 v')
      in if d12 /= 0 && d13 /= 0 && vz' == 0 && p12 == p13 then p13 else next) (0,0,0) velocities
    subCoord (x, y, z) (a, b, c) = (x - a, y - b, z - c)
part2 _ = error "not enough data"