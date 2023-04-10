multi :: Int -> Int -> Int
multi _ 0 = 0 
multi x y   | x < 0 && y >= 0 = x + multi x (y-1)
            | x >= 0 && y < 0 = (-x) + multi x (y+1)
            | x >= 0 && y >= 0 =  x + multi x (y-1)
            | x < 0 && y < 0 =  (-x) + multi x (y+1)


