main = do
    putStrLn("Введите город вылета:")
    dep <- getLine

    putStrLn("Введите город прилета:")
    arr <- getLine

    putStrLn("Перелет из города " ++ dep ++ " в город " ++ arr)