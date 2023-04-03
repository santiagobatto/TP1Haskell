absoluto :: Integer -> Integer
absoluto x | x > 0 = x
           | x < 0 = (-1) * x

esMultiploDe :: Integer -> Integer -> Bool
esMultiploDe x y = mod y x == 0

esBiciesto :: Integer -> Bool 
esBiciesto x = mod x 400 == 0 || mod x 4 == 0 && mod x 100 /= 0

maximo :: Integer -> Integer -> Integer -> Integer
maximo a b c = max (max a b) c

minimo :: Integer -> Integer -> Integer -> Integer
minimo a b c = min (min a b) c

dispersion :: Integer -> Integer -> Integer -> Integer
dispersion a b c = ((maximo a b c) - (minimo a b c))


celciusToFahr :: Float -> Float
celciusToFahr x = x * 1.8 + 32 

fahrToCelsius :: Float -> Float
fahrToCelsius x = ((x - 32) / 1.8)

haceFrioF :: Float -> Bool
haceFrioF x = x < 8

--TUPLAS

segundo3 :: (Integer, Integer, Integer) -> Integer
segundo3 (a, b, c) = b

ordena :: (Integer, Integer) -> (Integer, Integer)
ordena (a, b) = (min a b, max a b)

rangoPrecio :: Integer -> (Integer, Integer) -> String
rangoPrecio x (a, b) | x > max a b = "Remil caro"
                     | x < min a b && x >= 0 = "Muy barato"
                     | x <= max a b && x > min a b = "Hay que verlo bien"
                     | x < 0 = "Es imposible"

mayor3 :: (Integer, Integer, Integer) -> (Bool, Bool, Bool)
mayor3 (a, b, c) = (a > 3, b > 3, c > 3)

todosIguales :: (Integer, Integer, Integer) -> Bool
todosIguales (a, b, c) = a == b && b == c