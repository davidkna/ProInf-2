{-
Übungsblatt 8 28.07.15
Evan Seilz David Knaack
-}
--1.--
data Gewicht = Kilo Double | Pfund Double
    deriving (Show)

kilo2pfund::Gewicht->Gewicht
kilo2pfund (Kilo n) = Pfund (n*2.20462262)
kilo2pfund _  = error "Input has to be Kilo"


pfund2kilo::Gewicht->Gewicht
pfund2kilo (Pfund n) = Kilo (n*0.45359237)
pfund2kilo _ = error "Input has to be Pfund"


--2
data Complex = C Int Int
    deriving Show
cadd :: Complex -> Complex -> Complex
cadd (C a b) (C x y) = C (a + x) (b + y)

cmultiply :: Complex -> Complex -> Complex
cmultiply (C a b) (C x y) = C (a * x - b * y) (b * x + a * y)

--4.--

data Nat = Zero |S Nat
    deriving Show

add :: Nat->Nat->Nat
add a Zero = a
add a (S b) = add (S a) b

nsucc :: Nat -> Nat
nsucc = S

npred :: Nat->Nat
npred Zero = Zero
npred (S b) = b

nmult :: Nat->Nat->Nat
nmult _ Zero = Zero
nmult a (S b)= add a (nmult a b)

npow :: Nat->Nat->Nat
npow _ Zero = S Zero
npow a(S b) = nmult a (npow a b)

factorial :: Nat->Nat
factorial Zero = S Zero
factorial (S a) = nmult (S a) (factorial a)

lowTh :: Nat->Nat->Bool
lowTh a b = comp a b == LT

greatTh :: Nat -> Nat -> Bool
greatTh a b = comp a b == GT

gleich :: Nat -> Nat -> Bool
gleich a b = comp a b == EQ

comp :: Nat -> Nat -> Ordering
comp Zero Zero   = EQ
comp Zero _      = LT
comp _    Zero   = GT
comp (S a) (S b) = comp a b


data Zint = Z Nat Nat
    deriving Show
zadd:: Zint->Zint->Zint
zadd (Z a b) (Z c d) =Z (add a c) (add b d)

zsub::Zint->Zint->Zint
zsub (Z a b) (Z c d)=Z (add a d) (add b c)

zmult::Zint->Zint->Zint
zmult (Z a b)(Z c d) =Z(add(nmult a d)(nmult b c))(add(nmult a c)(nmult b d))

zsimplify::Zint->Zint
zsimplify(Z Zero b) = Z Zero b
zsimplify(Z a Zero) = Z a Zero
zsimplify(Z(S a)(S b))= zsimplify(Z a b)


-- 4a
ungerade :: Nat -> Bool
ungerade Zero = False
ungerade (S a) = not (ungerade a)

-- a - b => 0, wenn b>=a
cutSub :: Nat -> Nat -> Nat
cutSub Zero _      = Zero
cutSub a    Zero   = a
cutSub (S a) (S b) = cutSub a b

nmin :: Nat -> Nat -> Nat
nmin a b = if a `greatTh` b then a else b

-- 4b
nat2Int :: Nat -> Int
nat2Int Zero = 0
nat2Int (S a) = 1 + nat2Int a

int2Nat :: Int -> Nat
int2Nat 0 = Zero
int2Nat a = S (int2Nat (a - 1))

-- c
zneg :: Zint -> Zint
zneg a = zneg' $ zsimplify a
    where zneg' (Z a b) = Z b a

zabs :: Zint -> Zint
zabs x = zabs' (zsimplify x)
    where
        zabs' (Z x Zero) = Z Zero x
        zabs' z          = z

zpow :: Zint -> Nat -> Zint
zpow _ Zero = Z Zero (S Zero)
zpow (Z a b) (S p) = zmult (Z a b) (zpow (Z a b) p)

zint2Int :: Zint -> Int
zint2Int (Z a b) = nat2Int b - nat2Int a

int2Zint :: Int -> Zint
int2Zint x | x > 0 = Z Zero (int2Nat x)
           | otherwise = Z (int2Nat x) Zero