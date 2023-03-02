--vamos haciendo divisiones enteras de los segundos restantes entre el número de segundos en la unidad de tiempo actual para calcular cuántos de dicha unidad hay
--dichos segundos restantes los calculamos haciendo la misma división de antes pero en módulo
--usamos a, b, c, d y e para evitar hacer cálculos de más
tiempo :: (Int, Int, Int, Int, Int)
tiempo = let a = 60 in let b = 60*a in let c = 24*b in let d = 365*c in let e = (10^6) in (div e d, div (mod e d) c, div (mod (mod e d) c) b, div (mod (mod (mod e d) c) b) a, mod (mod (mod (mod e d) c) b) a)

--hacemos exactamente lo mismo que antes, solo que aquí e es una variable
tiempofun :: Int -> (Int, Int, Int, Int, Int)
tiempofun e = let a = 60 in let b = 60*a in let c = 24*b in let d = 365*c in (div e d, div (mod e d) c, div (mod (mod e d) c) b, div (mod (mod (mod e d) c) b) a, mod (mod (mod (mod e d) c) b) a)

--para ser bisiesto, tiene que ser múltiplo de 4, excepto si lo es de 100, en cuyo caso no lo es a no ser que lo sea de 400
bisiesto :: Integral a => a -> Bool
bisiesto x
 | mod x 400 == 0 || (mod x 4 == 0 && mod x 100 /= 0) = True
 | otherwise = False

--igual que antes pero con if-else
bisiestoif :: Integral a => a -> Bool
bisiestoif x = if mod x 400 == 0 || (mod x 4 == 0 && mod x 100 /= 0) then True else False

--usamos sum y length, el segundo con fromIntegral para mantener la coherencia entre los tipos
med :: Fractional a => [a] -> a
med xs = sum xs / fromIntegral (length xs)

--el caso base es un número de un solo dígito, el paso inductivo es sumar 1 y reducir el número en 1 dígito
digs :: Integral a => a -> a
digs x
 | div x 10 == 0 = 1
 | otherwise = 1 + digs (div x 10)

--el caso base es el propio número si solo tiene un dígito, y el paso recursivo suma el último dígito a la propia función con el número sin dicho dígito
--llamamos a digs, ya que la tenemos, para comprobar si tenemos que seguir con la recursión
sumadigs :: Integral a => a -> a
sumadigs x
 | digs x == 1 = x
 | otherwise = mod x 10 + sumadigs (div x 10)

--nos apoyamos en sumadigs para hacer la suma de los dígitos
--si es negativo lo invertimos y volvemos a llamar, si es menor que 10 lo devolvemos (CB) y si no, aplicamos la función a su suma de dígitos
reduccion :: Integral a => a -> a
reduccion x
 | x < 0 = reduccion (abs x)
 | x < 10 = x
 | otherwise = reduccion (sumadigs x)

--estricta en la x, porque si x es True, por la evaluación perezosa no se comprueba el valor de y y se devuelve True
disya :: Bool -> Bool -> Bool
disya x y
 | x == False && y == False = False
 | otherwise = True

--estricta en la y, como en la anterior pero al revés
disyb :: Bool -> Bool -> Bool
disyb x y
 | y == False && x == False = False
 | otherwise = True

--estricta en las dos porque forzamos a comprobar al menos la primera igualdad de las dos primeras guardas
disyc :: Bool -> Bool -> Bool
disyc x y
 | x == False && y == True = True
 | y == False && x == True = True
 | x == False = False
 | otherwise = True
