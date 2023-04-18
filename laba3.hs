-- Общий класс
class Figure a where
    area :: a -> Float
    perimetr :: a -> Float

-- Прямоугольник
data Rectangle = Rectangle {
    rSideA :: Float, rSideB :: Float
} deriving Show 

instance Figure Rectangle where
    area a = rSideA a * rSideB a
    perimetr a = 2 * (rSideA a + rSideB a)

-- Треугольник
data Triangle = Triangle {
    tSideA :: Float, tSideB :: Float, tSideC :: Float
} deriving Show

instance Figure Triangle where
    area a = let p = perimetr a / 2 in sqrt (p * (p - tSideA a) * (p - tSideB a) * (p - tSideC a))
    perimetr a = tSideA a + tSideB a + tSideC a

-- Круг
data Circle = Circle {
    r :: Float
} deriving Show

instance Figure Circle where
    area a = r a ^ 2 * pi
    perimetr a = 2 * pi * r a

-- Ромб
data Rhomb = Rhomb {
    rhSide :: Float,
    rhAngle :: Float
} deriving Show

instance Figure Rhomb where
    area a = (rhSide a ^ 2) * (sin (rhAngle a * pi/180))
    perimetr a = 4 * rhSide a