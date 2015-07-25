-- ProInformatik II: Funktionale Programmierung
-- 1. Übungsblatt für Wochenende
-- David Knaack und Evan Seilz

-- Aufgabe 1
-- Ansatz 1 mit Listengeneratoren (sehr langsam)
quadSolutions :: Int -> Int -> Int -> [Int]
quadSolutions a b c = [x | x <- [minBound :: Int .. maxBound :: Int], (a * x^2 + b * x + c) == 0]
-- Ansatz 2 mit pq Formel

-- Aufgabe 2
-- Typendeklaration
type Set = [Int]

-- Aufgabe 3

-- Aufgabe 4

-- Aufgabe 5

-- Aufgabe 6

-- Aufgabe 7
{-Hilfe für 7.

01010
10101
01010
10101
01010

foo::Int->Int->Int->Char
foo x y s = toEnum(48+(x+y)`mod`2))

001100
001100
110011
110011
001100


foo::Int->Int->Int->Char
foo x y s = toEnum(48+((x`div`w+y`div`w)`mod`2)
	where w = s`div`8

	-}
-- Vorgegebene Funktion
paintPicture :: ((Int, Int, Int) -> Char) -> Int -> [Char]
paintPicture f size = paint size (map f [(x,y,size) | x <- [1..size], y <- [1..size]])
                      where
                        paint 0  []     = []
                        paint 0 (c:cs)  = '\n' : (paint size (c:cs))
                        paint n (c:cs)  = c: (paint (n-1) cs)
