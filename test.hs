map2 :: (a -> a -> a) -> [a] -> [a] -> [a]
map2 _ [ ] _ = [ ]
map2 _ _ [ ] = [ ]
map2  f  (x:xs) (y:ys) = (f x y) : (map2 f xs ys)

ff2 x y = map2 (\a b -> a + b) x y

multiplyByN :: Integer -> (Integer -> Integer)
multiplyByN  n  =  \x -> n * x

add x n = x + n
flist  ::  [Int  ->  Int]
flist = map add [1,2,3]


addN  ::  Int  ->  [Int]
addN  n  =  map  (\f  ->  f  n)  flist
=