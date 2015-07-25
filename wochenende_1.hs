-- ProInformatik II: Funktionale Programmierung
-- 1. Übungsblatt für Wochenende
-- David Knaack und Evan Seilz

-- Aufgabe 1
-- Ansatz 1 mit Listengeneratoren (sehr langsam)
quadSolutions :: Int -> Int -> Int -> [Int]
quadSolutions a b c = [x | x <- [minBound :: Int .. maxBound :: Int], (a * x^2 + b * x + c) == 0]

-- Ansatz 2 mit pq Formel
quadSolutions' :: Double -> Double -> Double -> [Double]
-- Wandelt um in Form x^2 + p*x + q = 0
quadSolutions' 0 b c = [(-c)/b]
quadSolutions' a b 0 = [0]
quadSolutions' a b c = pq (b / a) (c / a)

pq :: Double -> Double -> [Double]
pq p q = case compare wurzelInnen 0 of
    -- wurzelInnen < 0
    LT -> []
    -- wurzelInnen == 0
    EQ ->  [(-pHalbe) + p * sqrt wurzelInnen]
    -- wurzelInnen > 0
    GT -> let pWurzel = p * sqrt wurzelInnen in [(-pHalbe) + pWurzel, (-pHalbe) - pWurzel]
    where
        pHalbe      = p / 2 
        wurzelInnen = pHalbe ^ 2 - q

-- Ansatz 3 mit Rückgabe vom Typ Int statt Double
-- Rückgabe wie in den Beispielen
quadSolutionsInt :: Int -> Int -> Int -> [Int]
quadSolutionsInt a b c = solutionsInt(quadSolutions' a' b' c')
    where
        -- Konvertiert Argumente zu Double
        a' = fromIntegral a
        b' = fromIntegral b
        c' = fromIntegral c
        -- Übernehme einzelne Elemente der Liste, wenn sie ganzzahlig sind
        solutionsInt :: [Double] -> [Int]
        solutionsInt [] = []
        solutionsInt (x:xs)
            | x - fromIntegral (floor x) == 0 = floor x : solutionsInt xs
            | otherwise                       = solutionsInt xs

                    
-- Aufgabe 2
-- Typendeklaration
type Set = [Int]

-- Mengendifferenz von a und b
-- Gibt alle Elemente zurück die in x aber nicht in y sind
mengendifferenz :: Set -> Set -> Set
mengendifferenz [] [] = []
mengendifferenz (x:xs) (y:ys) = case x `compare` y of
    -- x == y
    EQ -> mengendifferenz xs ys
    -- x < y
    LT -> x : mengendifferenz xs (y:ys)
    -- y > x
    GT -> x : mengendifferenz xs ys
mengendifferenz x  [] = x
mengendifferenz [] _  = []

-- Aufgabe 3
piRekursiv :: Integer -> Double
piRekursiv n = 4 * piRekursiv' n
    where
        piRekursiv' :: Integer -> Double
        piRekursiv' 0 = 1
        piRekursiv' k = ((-1) ** fromIntegral k) / (2 * fromIntegral k + 1) + piRekursiv (k - 1)

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
collatzSeq 1 = [1]
collatzSeq x = x : collatzSeq (nextCollatz  x)

-- 6c)
collatzSeqs :: Integer -> [[Integer]]
collatzSeqs n = [collatzSeq x | x <- [1 .. n]]
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

-- Prüft ob n zwischen x und y ist
zwischen :: Int -> (Int, Int) -> Bool
zwischen n (x, y) = x < n && y > n

-- Gibt ganzzahlige Entfernung zwischen zwei Punkten zurück
entfernung :: (Int, Int) -> (Int, Int) -> Int
entfernung (x1, y1) (x2, y2) = round (sqrt (fromIntegral (x' * x' + y' * y')))
	where
		x' = (x1 - x2)
		y' = (y1 - y2)

swissFlag :: (Int, Int, Int) -> Char
swissFlag (x , y, size)
    | x `zwischen` (einFünftel, size - einFünftel) && y `zwischen` (zweiFünftel, size - zweiFünftel) = ' '
    | y `zwischen` (einFünftel, size - einFünftel) && x `zwischen` (zweiFünftel, size - zweiFünftel) = ' '
    | otherwise = '.'
    where
        einFünftel  = size `div` 5
        zweiFünftel = 2 * einFünftel
        



circle :: (Int, Int, Int) -> Char
circle(x , y, size)
    | inCircle  = if x < mitte then ' ' else '-'
    | otherwise = if y > mitte then ':' else '.'
    where
        inCircle = entfernung (mitte, mitte) (x, y) <= 2 * size `div` 5
        mitte = size `div` 2

-- Zum Testen
testSwissFlag  = putStrLn (paintPicture swissFlag 60)
testCircle  = putStrLn (paintPicture circle 60)