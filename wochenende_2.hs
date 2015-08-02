-- ProInformatik II: Funktionale Programmierung
-- 2. Übungsblatt für Wochenende
-- David Knaack und Evan Seilz

-- Aufgabe 1

-- Aufgabe 2
quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) =
    quicksort [n | n <- xs, n <= x]  ++ [x] ++ quicksort [ n| n <- xs, n > x]  

minNatNotIn :: [Int] -> Int
minNatNotIn x = helper (quicksort x) [0..]
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

-- minNatNotIn wird 1 mal aufgerufen O(1)
-- Wenn n die Länge der Eingabelist ist, wird quickSort wird bis zu O(n) mal rekursiv aufgerufen, wenn sich die Liste immer nur um ein Element verkleinert und mit O(n) Vergleichen pro Aufrufeen
-- helper wird mit Vergleichen für bis zu jedes Element der Liste ausgeführt, hat also eine Komplexität von O (n)
-- Insgesmat hat minNatNotIn hier eine Zeitkomplexität von O(1 + n + n^2) = O(n^2) und mit einem anderm Algorithmus bis zu O(n*log(n))

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

-- Gibt Tupel aus neuer Queue und Element zurück
dequeue :: Queue a -> (Queue a, a)
dequeue (Q [] []) = error "Empty Queue"
dequeue (Q [] xs) =
    let rxs = reverse xs
    in (Q rxs [], head rxs)
dequeue (Q (x:xs) ys) = (Q xs ys, x) 
 
 -- Aufgabe 4
 {-
1) korrekt   Variable Z
2) inkorrekt
3) inkorrekt 
4) inkorrekt
5) korrekt keine Variable
6) inkorrekt
-}
