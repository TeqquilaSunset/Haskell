even_double :: [Int] -> Int -> [Int]
even_double [] _ = []
even_double [a] _ = [a]
even_double (h:end) n = h : [(head end) * n] ++ even_double (tail end)  n 

inter :: Eq a => [a] -> [a] -> [a]
inter a b = [x |x <- a, elem x b]