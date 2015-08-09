-- ProInformatik II: Funktionale Programmierung
-- 2. Übungsblatt für Wochenende
-- David Knaack und Evan Seilz


-- Aufgabe 1
-- a)
aufgabe1_a :: [Bool]
aufgabe1_a = iterate (\x -> not x) True

-- b)
aufgabe1_b :: [Integer]
aufgabe1_b = iterate (\x -> (-2) * x) 2

-- c) 
aufgabe1_c :: [Integer]
aufgabe1_c = map fst $ iterate (\(_, n) -> (n * (n + 1), n + 1))  (2, 1)

-- Aufgabe 2
-- Vorgegebene Funktion
unfold p f g x
    | p x = []
    | otherwise = f x : unfold p f g (g x)

map :: (a -> b) -> [a] -> [b]
-- (f . head) damit f immer nur auf Kopf der Liste angewendet wird
map f xs = unfold isEmpty (f . head) tail xs 
    where
        -- Zur Vermeidung von Begrezung auf Eq a
        isEmpty [] = True
        isEmpty _  = False

iterate :: (a -> a) -> a -> [a]
iterate f x = unfold (\_ -> False) (\x -> x) f x 

int2bin :: Int -> [Int]
int2bin 0 = [0]
int2bin x = unfold (== 0) (`mod` 2) (`div` 2) x

-- Aufgabe 3
-- Lambda Ausdruck:
-- \x.y(yxy)
-- Regel 6
-- S (\x.y) (\x.yxy)
-- S (\x.y) (\x.(yx)y)
-- Regel 4
-- S (Ky) (\x.(yx)y)
-- Regel 6
-- S (Ky) (S (\x.yx) (\x.y))
-- Regel 5
-- S (Ky) (S y (\x.y))
-- Regel 4
-- S (Ky) (S y (Ky))

-- Aufgabe 4

reverse :: [Liste a] -> [Liste a]
reverse L   = L
reverse NIL = NIL
reverse x   = head x : (reverse x (tail x))

(++) :: (Liste a) -> (Liste a) -> (Liste a)
(++) a b = aux (reverse a) b
    where
        aux NIL y   = y
        aux L   y   = y
        aux x   Nil = x
        aux y   L   = y
        aux x   y   = aux (tail x) (head x : y)
