multi :: Int -> Int -> Int
multi _ 0 = 0 
multi 0 _ = 0
multi x y   | x < 0 && y >= 0 = x + multi x (y-1)
            | x >= 0 && y < 0 = (-x) + multi x (y+1)
            | x >= 0 && y >= 0 =  x + multi x (y-1)
            | x < 0 && y < 0 =  (-x) + multi x (y+1)

mult :: Int -> Int -> Int
mult 0 _ = 0
mult _ 0 = 0
mult x y
  | y > 0 = x + mult x (y - 1)
  | y < 0 = - mult x (-y)
  | otherwise = 0

list :: [a] -> [[a]]
list s = [[x] | x <- s]