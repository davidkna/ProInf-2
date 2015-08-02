-- ProInformatik II: Funktionale Programmierung
-- 2. Übungsblatt für Wochenende
-- David Knaack und Evan Seilz


-- Aufgabe 1
-- a)
potenzNoRep :: Int -> Int -> [Int]
potenzNoRep n m = mergeSort [potenzList b m | b <- [2 .. n]]
  where
    -- gibt Liste mit allen Potenzen zu Basis b zurück
    -- jede potenzList ist von klein nach groß sortiert
    potenzList b m = [b ^ e | e <- [2 .. m]]

    -- mergeSort bis es nur noch eine List gibt
    mergeSort [xs] = xs
    mergeSort xs = mergeSort (mergeHelp xs)
    mergeHelp (x1:x2:xs) = merge x1 x2 : mergeHelp xs
    mergeHelp xs = xs

    -- merge ist verändert zum Entfernen von Duplikate
    merge xs [] = xs
    merge [] ys = ys
    merge (x:xs) (y:ys) = case x `compare` y of
      EQ -> x : merge xs ys
      LT -> x : merge xs (y:ys)
      GT -> y : merge (x:xs) ys

-- b)
-- b^e muss O(n*m) (für jedes n und m) ausgeführt werden
-- die resultierenden Listen haben insgesmat O(n*m) Elemente
-- Merge Sort (aus dem Unterricht) hat eine Zeitkomplexität von O(n*log(n)) abhing von der Listenlänge n
-- die Änderungen für das Entfernen doppelter Element ändern das nicht, also ist hat mergeSort eine Komplexität von O(n*m*log(n*m))
-- insgesamt ist die Laufzeit O(n*m + n*m*log(n*m)) also O(n*m*log(n*m))


-- Aufgabe 2
-- a)
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    quicksort [n | n <- xs, n <= x]  ++ [x] ++ quicksort [ n| n <- xs, n > x]

minNatNotIn :: [Int] -> Int
minNatNotIn l = helper (quicksort l) [0..]
    where
        helper :: [Int] -> [Int] -> Int
        helper [] y = head y
        helper (x:xs) (y:ys) = case compare x y of
            -- Die natürliche Zahl ist vorhanden
            EQ -> helper xs ys
            -- Es handelt sich nicht um eine natürliche Zahl
            LT -> helper xs (y:ys)
            -- Die natürliche Zahl ist die erste, die fehlt
            GT -> y
-- b)
-- minNatNotIn wird 1 mal aufgerufen O(1)
-- wenn n die Länge der Eingabeliste ist, wird quicksort wird bis zu O(n) mal rekursiv aufgerufen, wenn sich die Liste immer nur um ein Element verkleinert, wobei O(n) Vergleichen pro Aufruf durchgeführt werden - ingesamt also O(n^2)
-- helper wird mit Vergleichen für bis zu jedes Element der Liste ausgeführt, hat also eine Komplexität von O (n)
-- insgesmat hat minNatNotIn hier eine Zeitkomplexität von O(1 + n + n^2) = O(n^2) und mit einem anderm Algorithmus zum Sortieren mit Vergleichen bis zu O(n*log(n))

-- Aufgabe 3
-- Listen werden hier als Stapel verwendet
data Queue a = Q [a] [a] deriving Show

makeQueue :: Queue a
makeQueue = Q [] []

isEmpty :: Queue a -> Bool
isEmpty (Q [] []) = True
isEmpty _         = False

enqueue :: Queue a -> a -> Queue a
enqueue (Q a b) x = Q a (x:b)

-- Gibt Tupel mit neuer Queue und Element zurück
dequeue :: Queue a -> (Queue a, a)
dequeue (Q [] []) = error "Empty Queue"
dequeue (Q [] ys) = dequeue $ Q (reverse ys) []
dequeue (Q (x:xs) ys) = (Q xs ys, x)

-- Aufgabe 4
--     1) inkorrekt
--     2) inkorrekt
--     3) korrekt - y, z und w sind frei, Rest nicht frei
--     4) inkorrekt
--     5) korrekt - x, b und c sind frei, Rest nicht frei
--     6) korrekt - w und b sind frei

-- Aufgabe 5
-- 1.
--     (λx.xx) (λabc.cbabc) (λx.fxx)(fx)y
--     (λabc.cbabc) (λabc.cbabc) (λx.fxx)(fx)y
--     (λx.fxx) (fx) (λabc.cbabc) (fx) (λx.fxx)y
--     f(fx)(fx) y (λx.fxx) (fx) (λx.fxx) y
--     f(fx)(fx) y f(fx)(fx) (λx.fxx) y
--     ffxfxyffxfxfyy

-- 2.
--     (λx.x)((λabc.b(abc))(λfx.f(fx))))
--     (λt.t)((λabc.b(abc))(λfx.f(fx)))
--     (λabc.b(abc))(λfx.f(fx))
--     (λbc.b((λfx.f(fx))bc))
--     (λbc.b((λfx.f(fx))bc))
--     (λbc.bb(bc))
--     (λbc.bbbc)

-- Aufgabe 6
--     λwyx. y (w y x)  ≡S
--     (S (S λsx.s(x)))
--     (S (S λsq.s(q)))
--     (S (λwyx. y (w y x) λsq.s(1)))
--     (S (λyx. y (λsq.s(q) y x) ))
--     (S (λyx. yyx))
--     (S (λyx. ppq))
--     (λwyx. y (w y x)(λyx. ppq))
--     (λyx. yyyx)
