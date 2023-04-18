import Data.List (sortBy)
import Data.Function (on)

type Flight = (String, String, String, String, String) -- (рейс, пункт вылета, пункт прилета, время вылета, время прилета)

main = do
  contents <- readFile "flights.txt" -- чтение данных из файла
  let flights = map parseFlight (lines contents) -- преобразование строк в кортежи типа Flight
  let dep = "Moscow" -- заданный пункт вылета
  let arr = "New-York" -- заданный пункт прилета
  let possibleFlights = filter (\(f, d, a, _, _) -> d == dep && a == arr) flights -- фильтрация списка рейсов
  let sortedFlights = sortBy (compare `on` (\(_, _, _, depTime, arrTime) -> depTime ++ arrTime)) possibleFlights -- сортировка по времени перелета
  print sortedFlights

-- преобразование строки в кортеж типа Flight
parseFlight :: String -> Flight
parseFlight line = (f, d, a, depTime, arrTime)
  where [f, d, a, depTime, arrTime] = words line
