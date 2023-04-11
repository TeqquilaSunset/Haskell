f :: Int -> Int
f n | (n `mod` 3) /= 0 = 0
f n = sum [x*(x+1)*(x+2) | x <- [1,4..n]]

p :: [a] -> Int -> Int -> [a]
p [] _ _ = []
p s _ 0 = s
p (h:end) n1 n2 | n1 /= 0 = h:p end (n1-1) (n2-1)
                | n1 == 0 = p end n1 (n2-1)