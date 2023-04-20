import Data.List (sortBy)
import Data.Function (on)

type Flight = (String, String, String, String, String)

-- замена ":" на пробел
replaceColon :: String -> String
replaceColon "" = ""
replaceColon (x:xs) 
  | x == ':' = (' ':(replaceColon xs))
  | otherwise = (x:(replaceColon xs))

-- Проеверка на високосный год 
isLeapYear :: Int -> Bool
isLeapYear year
    | year `mod` 400 == 0 = True
    | year `mod` 100 == 0 = False
    | year `mod` 4 == 0 = True
    | otherwise = False

-- Количество дней с начала года на конец месяца
dayInMonth :: Int -> Int
dayInMonth month 
  | month == 0 = 0
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
  | otherwise = error "Unknown month"

-- Перевод строки "00:00:00:00:00" в секунды Int
toSeconds :: String -> Int
toSeconds str = let febDay = if isLeapYear year && month > 2 then 1 else 0 in
  (year - 1) * 31536000 + (dayInMonth (month - 1) + (day + febDay)) * 86400 + hour * 3600 + minut * 60
    where [year, month, day, hour, minut] = map read (words (replaceColon str))

-- Преобразование строки в кортеж типа Flight
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