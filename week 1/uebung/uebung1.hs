-- Aufgabe 1.1 Quadratische Funktion a*x^2 + b*x + c 
quadratic :: (Int, Int, Int) -> Int -> Int
quadratic (a, b, c) x = a * x^2 + b * x + c

-- Aufgabe 1.2 Square Funktion x^2
square :: Int -> Int
square x
    | x < 0 = square(-x)
    | x == 0 = 0
    | otherwise = 2 * x - 1 + square(x - 1) 

-- Aufgabe 1.3.a Summe aller Elemente einer Liste
sumList :: [Int] -> Int
sumList [] = 0
sumList (x:xs) = x + sumList(xs)

-- Aufgabe 1.3.b Alle Elemente mit übergebenen Operator
foldList :: (Double -> Double -> Double) -> [Double] -> Double
foldList _ [] = 0 
foldList f [x] = x 
foldList f (x:y:xs) = foldList f (f x y : xs) 

-- Aufgabe 1.3.c Funktion die Liste zurückgibt mapList 
mapList :: (Int -> Int) -> [Int] -> [Int]
mapList _ [] = []
mapList f (x:xs) = f x : mapList f xs

-- Aufgabe 1.4 
tableInt :: (Int -> Int) -> [Int] -> String
tableInt f xs = 