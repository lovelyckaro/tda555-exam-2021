module P2 where
nRoots :: Double -> Double -> Double -> Int
nRoots a b c 
  | discriminant == 0 = 1
  | discriminant > 0 = 2
  | otherwise = 0
    where discriminant = b^2 - 4 * a * c

data Root = None | One Double | Two Double Double

roots :: (Double, Double, Double) -> Root
roots (a,b,c) = case nRoots a b c of
  0 -> None
  1 -> One $ (-b) / (2 * a)
  2 -> Two ((-b + sqrt d) / (2 * a)) ((-b - sqrt d) / (2 * a))
    where d = b^2 - 4 * a * c
  _ -> error "unreachable"
   