{-
Übungsblatt 7 27.07.15
Evan Seilz David Knaack
-}
--1.--

{- 1. Ansatz
selectSort :: (Ord a) => (a -> a -> Bool) -> [a] -> [a]
selectSort operator liste = [getFirst liste]
	where
		operator' = operator . not  
		getFirst :: (Ord a) =>[a] -> a
		getFirst = foldr1 (\x acc -> if x `operator'` acc then x else acc) 
		delFirst :: (Ord a) => a -> [a] -> [a] 
		delFirst x (y:ys)
			| x == y = ys
			| otherwise = x : (delFirst x ys)
		delFirst _ _ = undefined
		-}
--2. Ansatz
{-

selectSort::(Ord a)=>(a->Bool)->[a]->[a]
selectSort f xs = helpSort xs [] f
	where
		helpSort[] xs _ = xs
		helpSort xs ys f = helpSort(delFirst xs f)((getFirst xs f):ys) f
		getFirst [] _ x = x
		getFirst (x:xs) f y
			|f x y = getFirst xs f y
			|otherwise = getFirst xs f x
		delFirst [] _ _ = []
		delFirst [_] _ _ = []
		delFirst (x:xs)
			|getFirst(x:xs) f x ==x =xs
			|otherwise = x:(delFirst xs f)



--2.--
--a)i

tails::[a]->[[a]]
tails [] = []
tails (x:xs) = (x:xs):tails xs

--a)ii

prefix::Eq a => [a]->[a]->[a]
prefix [] _ = []
prefix _ [] = []
prefix (x:xs)(y:ys)
	| x==y=x prefix xs ys
	| otherwise []

--a)iii


longestPrefix xs =sel(helper xs) 
	where
		helper xs | length xs < 2 = (0,[])
		helper (x:y:xs) = (length p,p):helper (y:xs)
			where
				p=prefix x y 
sel [x]=x 
sel ((l1,s1):(l2,s2):xs)
	| l1>=l2=sel((l1,s1):xs)
	|otherwise = sel ((l2,s2):xs)

longestRepSeq:: Eq a =>[a]->[a]
longestRepSeq xs = snd(longestPrefix(sort(<=)(tails xs)))

 3. 
Tails hat eine Komplexität von T(n)= O(n)
sort hat eine Komplexität von T(n)=O(n log n)*T<= =O(n²log n)
Prefix hat eine Komplexität von T(n)= O(n)
LongestPrefix hat eine Komplexität von T(n)=O(n²)
snd hat eine Komplexität von O(1)
-}
--4.--
flattenL::Num a =>[[a]] ->[a]
flattenL = foldl (++) []


flattenR::Num a =>[[a]] ->[a]
flattenR = foldr (++) []

{- Die Methode flattenR ist schneller als die methode FlattenL 
in der Ausführung jedoch sollte vom Speicherplatz, die foldL methode 
sinnvoller sein da dadurch nicht ewige rekursive Verkettungen 
entstehen sondern ein Zwischenergebnis sofort ausgerechnet wird-}

--mymin:: Num a => [a] -> a
mymin (x:y:xs) a = foldr (if x<y then x else y)


--7.-
bin2dec::[Int]-> Int
bin2dec = foldl help 0
	where help y x = y*2+x