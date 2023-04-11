three :: [[Int]] -> [Int]
three [] = []
three [_] = []
three s = deldouble $ double [x | x <- concat s, x `mod` 3 == 0]

double  :: [Int] -> [Int] -- Возвращает список с элементами >1
double s = [x | x <- s, (length (filter (==x) s)) > 1]

deldouble :: [Int] -> [Int] -- Устраняет дублирование
deldouble [] = []
deldouble (h:end)   | elem h end = deldouble end
                    | otherwise = h:deldouble end

----------------------------------------------
f :: (a -> Bool) -> [[a]] -> Int -> Bool
f p s n =  (length  $ filter p (concat s)) == n                        

-- Пример вызова f (\x -> x > 0) [[1,2], [3,7]] 4 вернет True
-- или f (\x -> x == "H") [["H","B"],["A"]] 1 вернет True