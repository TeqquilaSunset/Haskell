import Data.List (sortBy)
import Data.Function (on)

type Flight = (String, String, String, String, String)

-- замена : на пробел
replaceColon :: String -> String
replaceColon "" = ""
replaceColon (x:xs) 
  | x == ':' = (' ':(replaceColon xs))
  | otherwise = (x:(replaceColon xs))

-- Перевод "00:00" в секунды
toSeconds :: String -> Int
toSeconds str = hours * 3600 + minuts * 60
  where [hours,minuts] = map read (words (replaceColon str))

-- преобразование строки в кортеж типа Flight
parseFlight :: String -> Flight
parseFlight line = (f, d, a, depTime, arrTime)
  where [f, d, a, depTime, arrTime] = words line

main = do
  contents <- readFile "flights.txt" -- чтение данных из файла
  let flights = map parseFlight (lines contents) -- преобразование строк в кортежи типа Flight
  
  putStrLn("Введите город вылета:")
  dep <- getLine

  putStrLn("Введите город прилета:")
  arr <- getLine

  let possibleFlights = filter (\(_, d, a, _, _) -> d == dep && a == arr) flights -- фильтрация списка рейсов
  let sortedFlights = sortBy (compare `on` (\(_, _, _, depTime, arrTime) -> toSeconds arrTime - toSeconds depTime )) possibleFlights -- сортировка по времени перелета
  print sortedFlights