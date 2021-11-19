thrice x = [x,x,x]

sums (x:y:ys) = x : sums (x + y : ys)
sums xs = xs

{-
thrice (sums [0..4]) =
  thrice (sums (0:1:[2..4])) =
  thrice (0 : sums (1 : 2 : [3..4])) =
  thrice (0 : 1 : sums (3 : 3 : 4 : [])) =
  thrice (0 : 1 : 3 : sums (6 : 4 : [])) =
  thrice (0 : 1 : 3 : 6 : sums (10 : 4 : [])) =
  thrice (0 : 1 : 3 : 6 : 10 : [])
  thrice [0, 1, 3, 6, 4] =
  [[0, 1, 3, 6, 4], [0, 1, 3, 6, 4], [0, 1, 3, 6, 4]]

>>> thrice (sums [0..4])
[[0,1,3,6,10],[0,1,3,6,10],[0,1,3,6,10]]
-}

