import Data.List (sortBy)
import Data.Function (on)

type Flight = (String, String, String, String, String)

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
  let sortedFlights = sortBy (compare `on` (\(_, _, _, depTime, arrTime) -> depTime ++ arrTime)) possibleFlights -- сортировка по времени перелета
  print sortedFlights

