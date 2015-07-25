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

-- Mengendifferenz von a und b
-- Gibt alle Elemente zurück die in x aber nicht in y sind
mengendifferenz :: Set -> Set -> Set
mengendifferenz [] [] = []
mengendifferenz (x:xs) (y:ys) = case x `compare` y of
    EQ -> mengendifferenz xs ys
    LT -> x : mengendifferenz xs (y:ys)
    GT -> x : mengendifferenz xs ys
mengendifferenz x  [] = x
mengendifferenz [] _  = []

-- Aufgabe 3
piRekursiv :: Integer -> Double
piRekursiv n = 4 * piRekursiv' n
	where
		piRekursiv' :: Integer -> Double
		piRekursiv' 0 = 0.0
		piRekursiv' k = ((-1) ** (fromIntegral (k))) / (2.0 * fromIntegral k + 1) + piRekursiv (k - 1)

piListe :: Integer -> Double
piListe n = 4 * sum [((-1) ** fromIntegral k) / (2 * fromIntegral k + 1) | k <- [0 .. n]]

-- Aufgabe 4

echtTeiler :: Int -> [Int]
echtTeiler n = [x | x <- [1 .. (n `div` 2)], n `mod` x == 0]

-- Aufgabe 5

-- Aufgabe 6
-- 6a)
nextCollatz :: Integer -> Integer
nextCollatz n | n `mod` 2 == 0 = n `div` 2
			  | otherwise      = n * 3 + 1

-- 6b)
collatzSeq :: Integer -> [Integer]
collatzSeq 1 = []
collatzSeq x = x : collatzSeq (nextCollatz  x)

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
