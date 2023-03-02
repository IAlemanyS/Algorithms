-------------------1

type Punto = (Int, Int)
data Dir = Arriba Int | Abajo Int | Izda Int | Dcha Int deriving (Show, Eq, Ord)

mueve :: Punto -> Dir -> Punto
mueve (x, y) (Arriba z) = (x, min 100 (y + z))
mueve (x, y) (Dcha z) = (min 100 (x + z), y)
mueve (x, y) (Abajo z) = (x, max 0 (y - z))
mueve (x, y) (Izda z) = (max 0 (x - z), y)

destino :: Punto -> [Dir] -> Punto
destino p ms = foldl mueve p ms

trayectoria :: Punto -> [Dir] -> [Punto]
trayectoria p ms = foldl (\xs y -> xs ++ [mueve (last xs) y]) [p] ms

-------------------2

data Nat = Cero | Suc Nat deriving (Eq, Ord)

infixl 6 +#
n +# Cero = n
Cero +# n = n
n +# Suc m = Suc (n +# m)

infixl 7 *#
Cero *# _ = Cero
_ *# Cero = Cero
n *# Suc m = n +# n *# m

natToInt :: Nat -> Int
natToInt Cero = 0
natToInt (Suc n) = 1 + natToInt n

instance Show Nat where
 show n = show (natToInt n)

-----------------3

data Complex = C (Float, Float) deriving Eq

instance Show Complex where
 show (C (a, b)) = show a ++ (if b > 0 then " + " else " - ") ++ show (abs b) ++ "i"

instance Num Complex where
 C (a, b) + C (c, d) = C (a + b, c + d)
 C (a, b) - C (c, d) = C (a - b, c - d)
 C (a, b) * C (c, d) = C (a * c - b * d, a * d + b * c)

instance Fractional Complex where
 C (a, b) / C (c, d) = C ((a * c + b * d) / (c ^ 2 + d ^ 2), (b * c - a * d) / (c ^ 2 + d ^ 2))
 
----------------4

class Medible a where
 medida :: a -> Int

instance Medible Bool where 
 medida True = 1
 medida False = 0

instance Medible Int where
 medida x = abs x

instance Medible a => Medible [a] where
 medida[] = 0
 medida(x:xs) = medida x + medida xs

instance (Medible a, Medible b) => Medible (a,b) where
 medida(x,y) = medida x + medida y 
