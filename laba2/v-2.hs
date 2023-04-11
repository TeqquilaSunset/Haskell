f :: [Int] -> Int -> Int -> [[Int]]
f a b c = [[x | x <- a, even x]] ++ [[y | y <- a,  (floor(sqrt (fromIntegral y))) ^ 2 == y]] ++ [[z | z <- a, b <= z && z <= c]]
--f [1,2,3,4] 0 2

------------------------------------------
f_map :: Integer -> [(Integer -> Integer)] -> [Integer] -> [Integer]
f_map 1 (x:_) s = map x s
f_map n (x:xs) s = f_map (n-1) xs s
-- f_map 1 [(\x-> x^2), (\x->x+x), succ] [1,2,3,4,5,6]