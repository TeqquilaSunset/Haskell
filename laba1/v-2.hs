rep :: a -> Int -> [a]
rep _ 0 = []
rep x n = [x | _ <- [1..n]]

swap :: Eq a => a -> a -> [a] -> [a]
swap x y [] = []
swap x y (h:end)    | h == x = y:swap x y end
                    | h == y = x:swap x y end
                    | otherwise = h:swap x y end