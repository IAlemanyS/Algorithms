--------------------Ejercicio 1

--usamos head xs como valor cualquiera del tipo a
milast :: [a] -> a
milast xs = foldl (\x y -> y) (head xs) xs

mireverse :: [a] -> [a]
mireverse xs = foldl (\ys y -> [y] ++ ys) [] xs

miall :: (a -> Bool) -> [a] -> Bool
miall p xs = foldl (\x y -> x && p y) True xs

--usamos head xs como primer valor para calcular el mínimo
mimin :: Ord a => [a] -> a
mimin xs = foldl min (head xs) xs

mimap :: (a -> b) -> [a] -> [b]
mimap f xs = foldl (\ys y -> ys ++ [f y]) [] xs

mifilter :: (a -> Bool) -> [a] -> [a]
mifilter p xs = foldl (\ys y -> if p y then ys ++ [y] else ys) [] xs

mitakeWhile :: (a -> Bool) -> [a] -> [a]
mitakeWhile p xs = foldr (\y ys -> if p y then y:ys else []) [] xs

--------------------Ejercicio 2

lasfd :: [Int]
lasfd = foldl (\xs x -> xs ++ [x] ++ [-x]) [] [1..100]

--------------------Ejercicio 3

--cada diagonal suma 1 más que la anterior
paresDiag :: [(Int, Int)]
paresDiag = [(x, y) | n <- [0..], x <- [0..n], y <- [0..n], x + y == n]

--------------------Ejercicio 4

sufijos :: [a] -> [[a]]
sufijos xs = [reverse (take n (reverse xs)) | n <- [0..length xs]]

sublistas :: [a] -> [[a]]
sublistas xs = [take n (drop m xs) | n <- [0..length xs], m <- [0..min (length xs - n) n]]

--aplicamos recursivamente la función pera a xs, tomamos [head] y lo aplicamos al resto, etc
permuta :: Eq a => [a] -> [[a]]
permuta xs
 | xs == [] = [[]]
 | otherwise = concat [map ([head ys]++) (permuta (tail ys)) | ys <- map (pera (xs)) [0..length xs - 1]]

--pera nos devuelve la lista con el n-ésimo elemento movido al principio
pera :: [a] -> Int -> [a]
pera xs n = [head (drop n xs)] ++ take n xs ++ drop (n + 1) xs

--repite algunos cambiados de orden
--con n en lugar de (div n 2) repetía más
sumandos :: Int -> [[Int]]
sumandos n
 | n == 1 = [[1]]
 | n > 1 = aux n (div n 2) ++ [[n]] 

aux :: Int -> Int -> [[Int]]
aux n j
 | j == 0 = []
 | j > 0 = aux n (j - 1) ++ (map (\xs -> xs ++ [j]) (sumandos (n - j)))
