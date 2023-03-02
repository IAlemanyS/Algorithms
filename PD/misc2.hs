p :: Int -> Bool
p n = if n == 0 then True else i (n-1)
i :: Int -> Bool 
i n = if n == 0 then False else p (n - 1)

f :: Integral a => a -> [a]
f n
 | n == 0 = [0]
 | otherwise = f (n - 1) ++ [n ^ 2] ++ []

g :: Integral a => a -> [(a,a)]
g n
 | n == 0 = (0,0) : []
 | otherwise = (n, (n ^ 2)) : g(n - 1)

h :: (Eq a, Floating a) => a -> a
h n
 | n == 1 = cos 1
 | otherwise = n*abs(cos n) + h (n - 1)

j :: Integral a => a -> a
j n
 | n == 1 = 0
 | mod n 3 == 0 || mod n 5 == 0 = n + j (n - 1)
 | otherwise = j (n - 1)

pot :: Integral a => a -> a -> a 
pot n m
 | 3^m >= n = 0
 | mod (3^m) 100 == 43 = 1 + pot n (m + 1)
 | otherwise = pot n (m + 1)

k :: Integral a => a -> a
k n = pot n 1

zip3 :: [a] -> [b] -> [c] -> [(a, b, c)]
zip3 x y z = map (\((x, y), z) -> (x, y, z)) (zip (zip x y) z)

imparesEn :: Integral a => [a] -> [a]
imparesEn xs = filter odd xs

escalar :: Num a => [a] -> [a] -> a
escalar xs ys = sum (zipWith (*) xs ys)

mcdList :: Integral a => [a] -> a
mcdList xs = head (reverse [x | x <- [1 .. maximum xs], all (\y -> mod y x == 0) xs])

---------------hoja4

--1
data Pila a = P[a] deriving (Show, Eq)

creaPila :: Pila a
creaPila = P[]

esPilaVacia :: Eq a => Pila a -> Bool
esPilaVacia p = p == P[]

apilar :: a -> Pila a -> Pila a
apilar x (P(xs)) = P(x:xs)

cima :: Pila a -> a
cima (P[]) = error "La pila está vacía"
cima (P(xs)) = head xs

desapilar :: Pila a -> Pila a
desapilar (P(xs)) = P(tail xs)

r :: [a] -> [a] -- da la vuelta a una lista apilando sus elementos de izda a dcha y reconvirtiéndolo en lista
r xs = ys where P ys = foldl (\p x -> apilar x p) creaPila xs

--2

mayorQueCumple :: Integral a => (a -> Bool) -> a -> a -> Maybe a
mayorQueCumple p n m
 | xs == [] = Nothing
 | otherwise = Just (last xs)
 where xs = [x | x <- [n..m], p x]

--3

data Set a = S[a] deriving Show

instance Ord a => Ord (Set a) where
 (S(xs)) <= m = all (\x -> belongs x m) xs
 m < (S(xs)) = m <= (S(xs)) && any (\x -> not (belongs x m)) xs

instance (Eq a, Ord a) => Eq (Set a) where
 m == n = m <= n && n <= m

--pertenece
belongs :: Eq a => a -> Set a -> Bool
belongs x (S(xs)) = any (x==) xs

--bienDef
minElem :: Ord a => Set a -> a
minElem (S []) = error "Empty set"
minElem (S(xs))
 | length xs == 1 = head xs
 | otherwise = min (minElem (S(tail xs))) (head xs)

maxElem :: Ord a => Set a -> a
maxElem (S []) = error "Empty set"
maxElem (S(xs))
 | length xs == 1 = head xs
 | otherwise = max (maxElem (S(tail xs))) (head xs)

bienDef :: (Enum a, Ord a) => Set a -> Set a
bienDef m = S [x | x <- [(minElem m)..(maxElem m)], belongs x m]

--contenido
cont :: (Ord a, Enum a) => Set a -> Set a -> Bool
cont m n = bienDef m <= bienDef n

--4

data Temp = Kelvin Float | Celsius Float | Fahrenheit Float deriving Show

instance Eq Temp where
 Kelvin k == Kelvin k' = k == k'
 a == b = toKelvin a == toKelvin b

instance Ord Temp where
 Kelvin k < Kelvin k' = k < k'
 a < b = toKelvin a < toKelvin b
 Kelvin k > Kelvin k' = k > k'
 a > b = toKelvin a > toKelvin b
 a <= b = a < b || a == b
 a >= b = a > b || a == b

toCelsius :: Temp -> Temp
toCelsius (Celsius c) = Celsius c
toCelsius (Kelvin k) = Celsius (k - 272.15)
toCelsius (Fahrenheit f) = Celsius ((f - 32) * (5 / 9))

toKelvin :: Temp -> Temp
toKelvin (Kelvin k) = Kelvin k
toKelvin (Celsius c) = Kelvin (c + 272.15)
toKelvin (Fahrenheit f) = toKelvin (toCelsius (Fahrenheit f))

toFahrenheit :: Temp -> Temp
toFahrenheit (Fahrenheit f) = Fahrenheit f
toFahrenheit (Celsius c) = Fahrenheit ((c * 9) / 5 + 32)
toFahrenheit (Kelvin k) = toFahrenheit (toCelsius (Kelvin k))

--5

data Arbol a b = Hoja a | Nodo b (Arbol a b) (Arbol a b) deriving Show

--instance (Eq a, Eq b) => (Arbol a b) where
-- Hoja x == Hoja y = x == y
-- Nodo x y z == Nodo x' y' z' = x == x' && y == y' && z == z'

---------------hoja5 

getInt :: IO Int
getInt = do line <- getLine
            return (read line::Int)

--prommedia :: IO ()
promedia = do x <- getInt; print x; medias 1 x
--medias ::
medias i m = do x <- getInt; if x == -1 then return () else print "hola"; medias (i + 1) ((x + (m * i)) / (i + 1))
