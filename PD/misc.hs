numCuentaPos :: Ord a => [a] -> (Int, [Int])
numCuentaPos xs = aux xs 1
aux :: Ord a => [a] -> Int -> (Int, [Int])
aux (x:xs) n
 | (x:xs) == [] = (0, [])
 | (x:xs) == [x] = (0, [])
 | otherwise = if x < head xs then (1 + fst p, (snd p) ++ [n]) else p where
  p = aux xs (n + 1)

--maxterna :: Num a => [a] -> (Int, Int, Int)
maxterna xs = (i, i+1, i+2) where i = posmax [xs !! j + xs !! (j+1) + xs !! (j+2) | j <- [0..length xs - 3]]

--posmax :: Num a => [a] -> Int
posmax l = (!! 1) (foldl (\[x, i, j] y -> if y > x then [y, j+1, j + 1] else [x, i, j+1]) [head l, 0, 0] (tail l))

digitParImpar :: Integral a => a -> ([a], [a])
digitParImpar x
 | x == 0 = ([], [])
 | even (x `mod` 10) = (fst w ++ [x `mod` 10], snd w)
 | otherwise = (fst w, snd w ++ [x `mod` 10]) where w = digitParImpar (x `div` 10)

data Conjunto a = C([a], Int) deriving Show
intersec :: Eq a => Conjunto a -> Conjunto a -> Conjunto a
intersec (C((x:xs), n)) (C(l,m))
 | n == 0 || m == 0 || xs == [] = C([], 0)
 | otherwise = if elem x l then C(x:s, k+1) else C(s,k) where C(s,k) = intersec (C(xs,n-1)) (C(l,m))

interseca :: Eq a => ([a], Int) -> ([a], Int) -> ([a], Int)
interseca ((x:xs), n) (l,m)
 | n == 0 || m == 0 || xs == [] = ([], 0)
 | otherwise = if elem x l then (x:s, k+1) else (s,k) where (s,k) = interseca (xs,n-1) (l,m) 

trozos :: Eq a => [a] -> [a] -> [a] -> Bool
trozos xs ys zs = inix /= -1 && iniy /= -1 && inix /= finy && iniy /= finx where (inix, finx) = sublist xs zs; (iniy, finy) = sublist ys zs

sublist :: Eq a => [a] -> [a] -> (Int, Int)
sublist xs zs = (i, i + l) where i = snd(head(filter (\w -> fst w == xs) [(take l (drop k zs), k) | k <- [0..length zs - l]])); l = length xs

--tramos :: Ord a => [a] -> Int
--tramos [_] = 1
--tramos [_,_] = 1
--tramos (x:xs) = tramosaux (xs, u) where (if x < head xs then u = 1 else u = 0)
--tramosaux :: Ord a => ([a], Int) -> Int
--tramosaux ((x:xs), u)
-- | xs == [] = 1
-- | otherwise = if (x <= y && u ==1) || (x >= y && u ==0) then tramosaux (xs, u) else tramosaux (xs, mod (u+1) 2) + 1 where y = head xs

idiv :: Integral a => a -> [a]
idiv x = [n|n<-[1..x], odd n, divisor n x]
divisor :: Integral a => a -> a -> Bool
divisor a b = not (null [i |i<-[1..b], i*a==b])

idivs :: Integral a => [a] -> [a]
idivs ns = quitareps [] (concat [idiv n|n<-ns])
quitareps::Integral a =>[a]->[a]->[a]
quitareps xs [] = xs
quitareps xs (y:ys)
 | elem y xs = quitareps xs ys
 | otherwise = quitareps (y:xs) ys

argo (x:xs)
 | (x:xs) == [] = error "vacio"
 | True = x
