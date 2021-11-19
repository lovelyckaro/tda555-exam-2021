condMap1,condMap2,condMap3  :: (a -> Bool) -> (a -> a) -> [a] -> [a]
condMap1 p f [] = []
condMap1 p f (x:xs) | p x = f x : condMap1 p f xs
                    | otherwise = x : condMap1 p f xs

condMap2 p f xs = [if p x then f x else x | x <- xs]

condMap3 p f = map (\x -> if p x then f x else x)

replace :: Eq a => a -> a -> [a] -> [a]
replace target replacement = condMap1 (==target) (const replacement) 