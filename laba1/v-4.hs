swap :: [a] -> [a]
swap []=[]
swap [x]=[]
swap (h:end) = (head end :[h]) ++ swap (tail end)

comb :: Eq a => [a] -> [a] -> [a]
comb a b = [x |x <- a, not(elem x b)] ++ b