-- ProInformatik II: Funktionale Programmierung
-- 1. Übungsblatt für Wochenende
-- David Knaack und Evan Seilz

-- Aufgabe 1
-- Ansatz 1 mit Listengeneratoren (sehr langsam)
quadSolutions :: Int -> Int -> Int -> [Int]
quadSolutions a b c = [x | x <- [minBound :: Int .. maxBound :: Int], a * x^2 + b * x + c == 0]
-- Ansatz 2 mit pq Formel

-- Aufgabe 2

-- Aufgabe 3

-- Aufgabe 4

-- Aufgabe 5

-- Aufgabe 6

-- Aufgabe 7

-- Vorgegebene Funktion
paintPicture :: ((Int, Int, Int) -> Char) -> Int -> [Char]
paintPicture f size = paint size (map f [(x,y,size) | x <- [1..size], y <- [1..size]])
                      where
                        paint 0  []     = []
                        paint 0 (c:cs)  = '\n' : (paint size (c:cs))
                        paint n (c:cs)  = c: (paint (n-1) cs)
