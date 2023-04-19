import Data.List (sortBy)
import Data.Function (on)

type Flight = (String, String, String, String, String)

-- замена : на пробел
replaceColon :: String -> String
replaceColon "" = ""
replaceColon (x:xs) 
  | x == ':' = (' ':(replaceColon xs))
  | otherwise = (x:(replaceColon xs))

dayInMonth :: Int -> Int
dayInMonth month 
  | month == 1 = 31
  | month == 2 = 59
  | month == 3 = 90
  | month == 4 = 120
  | month == 5 = 151
  | month == 6 = 181
  | month == 7 = 212
  | month == 8 = 243
  | month == 9 = 273
  | month == 10 = 304
  | month == 11 = 334
  | month == 12 = 365

-- Перевод "00:00" в секунды
toSeconds :: String -> Int
toSeconds str = (year - 1) * 31536000 + (dayInMonth (month - 1) + day) * 86400 + hour * 3600 + minut * 60
  where [year, month, day, hour, minut] = map read (words (replaceColon str))

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