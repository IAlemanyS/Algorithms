---------------1

adivina :: Int -> IO ()
adivina n = do
 y <- intento n
 putStrLn "Correcto!"
 return ()

intento :: Int -> IO Int
intento n = do
 putStrLn "Intenta adivinar el numero:"
 num <- getLine
 let y = read num :: Int
 if y == n
  then
   return y
  else do
   if y < n then
    putStrLn "Es mayor"
   else
    putStrLn "Es menor"
   z <- intento n
   return n

---------------2

data Matriz = M [[Float]] deriving Show

escribeMatriz :: IO Matriz
escribeMatriz = do
 putStrLn "Dimension x de la matriz:"
 n <- getLine
 let x = read n :: Int
 putStrLn "Dimension y de la matriz:"
 n <- getLine
 let y = read n :: Int
 putStrLn "Valores:"
 m <- introduceColumna x y
 return (M m) 

--lee un Float del teclado y lo devuelve
introduceValor :: IO Float
introduceValor = do
 n <- getLine
 let v = read n :: Float
 return v

--devuelve una lista de Floats escritos por teclado vía introduceValor
introduceFila :: Int -> IO [Float]
introduceFila i
 | i == 0 = do return []
 | otherwise = do
  a <- introduceValor
  b <- introduceFila (i - 1)
  return (a:b)

--devuelve una lista de listas de Floats a través de varias llamadas a introduceFila
introduceColumna :: Int -> Int -> IO [[Float]]
introduceColumna i j
 | j == 0 = return []
 | otherwise = do
 a <- introduceFila i
 b <- introduceColumna i (j - 1)
 return (a:b)

dibujaMatriz :: Matriz -> IO ()
dibujaMatriz (M []) = do return ()
dibujaMatriz (M xs) = do
 let ys = head xs
 a <- dibujaFila ys
 b <- dibujaMatriz (M (tail xs))
 return ()

--imprime una fila por pantalla
dibujaFila :: [Float] -> IO ()
dibujaFila ys = do
 print ys
 return ()

---------------3

formatea :: String -> String -> Int -> IO ()
formatea fileIn fileOut n = do
 texto <- readFile fileIn
 textoJustificado <- justifica texto n
 a <- writeFile fileOut textoJustificado
 return ()

--va extrayendo lineas del texto y justificándolas una por una, para después volver a concatenarlo todo
justifica :: String -> Int -> IO String
justifica texto n
 | texto == "" = do return ""
 | otherwise = do
  let l = lines texto
  let t = unwords (words (l !! 0)) --primera línea, quitando espacios de mas
  let e = length (words (l !! 0)) - 1 --nº de bloques de espacios
  a <- justificaLinea t (n - length t) 0 e
  b <- justifica (unlines (drop 1 l)) n --recursión con el resto de líneas del texto
  return (a ++ b)

--va llamando a agregaEspacio en el siguiente bloque de espacios hasta que la longitud sea la deseada
--m: long a aumentar. i: siguiente espacio a aumentar. e: nº de bloques de espacios
justificaLinea :: String -> Int -> Int -> Int -> IO String
justificaLinea texto m i e
 | m <= 0 || e == 0 = do return (texto ++ "\n") --si no hay espacios, no lo toca
 | otherwise = do
  let t = agregaEspacio texto i 0
  let r = if i == e - 1 then 0 else i + 1
  k <- justificaLinea t (m - 1) r e
  return k

--agrega un espacio junto al i-ésimo espacio del texto. j indica el carácter que estamos leyendo en cada momento
agregaEspacio :: String -> Int -> Int -> String
agregaEspacio texto i j
 | texto !! j == ' ' && i == 0 = take j texto ++ " " ++ drop j texto
 | texto !! j == ' ' && texto !! (j + 1) /= ' ' && i > 0 = agregaEspacio texto (i - 1) (j + 1)
 | otherwise = agregaEspacio texto i (j + 1)
