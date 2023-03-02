---------------1 DEFINICIÓN DE LA ESTRUCTURA HASH---------------

data Hash = H [[(String, String)]]



---------------2 DECLARACIÓN COMO INSTANCIA DE SHOW---------------

instance Show Hash where
 show (H l) = "---\n" ++ showHash l ++ "---"

--zipeamos la tabla hash con los números correspondientes, los formateamos con showRow y después los concatenamos
showHash :: [[(String, String)]] -> String
showHash l = concat (map showRow (zip l [0..length l]))

--showRow recibe un par (fila de la tabla hash, entero) y lo imprime formateado
showRow :: ([(String, String)], Int) -> String
showRow (x, n) = "|" ++ show n ++ "| -> " ++ show x ++ "\n"



---------------3 DEFINICIÓN DE LA FUNCIÓN HASH---------------

--convertimos el primer carácter de la palabra a su valor ascii y le aplicamos el módulo 10
hash :: String -> Int
hash p = mod (fromEnum (head p)) 10



---------------4 INICIALIZACIÓN DEL DICCIONARIO---------------

--lee los datos del fichero y los mete en un diccionario inicialmente vacío
initialize :: String -> IO Hash
initialize filein = do
 t <- readFile filein
 return (addLines (lines t) initDict)

--recorre uno por uno los pares (palabra, traducción) y los va añadiendo al diccionario
addLines :: [String] -> Hash -> Hash
addLines l h
 | l == [] = h
 | otherwise = addPair (addLines (tail l) h) (head w) (last w) where w = words (head l)

--diccionario inicial vacío
initDict :: Hash
initDict = H [[], [], [], [], [], [], [], [], [], []]

--inserta un par (palabra, traducción) (que le llega separado) a un hash dado, concatenándolo al final de la lista que le corresponde
addPair :: Hash -> String -> String -> Hash
addPair (H l) s e = H (take n l ++ [(l !! n) ++ [(s, e)]] ++ drop (n + 1) l) where n = hash s



---------------5 LECTURA DE PALABRAS POR TECLADO---------------

readWords :: IO [String]
readWords = do
 t <- getLine
 return (words t)



---------------6 CÁLCULO DE SU LONGITUD MEDIA---------------

--lo he definido para una lista de palabras cualquiera; ya más adelante la llamamos con las introducidas por teclado
avgLen :: [String] -> Float
avgLen l = (fromIntegral (sum [length s | s <- l])) / fromIntegral (length l)



---------------7 BÚSQUEDA DE LA TRADUCCIÓN---------------

--recibe una lista de palabras y devuelve la lista de sus traducciones - más adelante la llamamos con las introducidas por teclado
translate :: [String] -> Hash -> [String]
translate l h = map (translateWord h) l

--busca directamente en la fila correspondiente de la tabla hash, pero una vez allí tiene que hacer un filter
translateWord :: Hash -> String -> String
translateWord (H l) s = if w == [] then "¡¡Palabra desconocida!!" else snd (head w) where w = filter (\(x,y) -> x == s) (l !! (hash s))



---------------8 MOSTRADO DE TRADUCCIONES---------------

--inicializa el diccionario a partir del nombre de fichero y llama a translations con él
dictionary :: IO ()
dictionary = do
 putStrLn "Introduce el nombre del fichero diccionario:"
 x <- getLine
 h <- initialize x
 translations h
 return ()

--mientras el usuario quiera, se van pidiendo listas de palabras y mostrando sus traducciones y longitud media
translations :: Hash -> IO ()
translations h = do
 putStrLn "\n¿Quieres ver el diccionario (d) o traducir palabras (t)?"
 x <- getLine
 if head x == 't'
  then do
   putStrLn "\nIntroduce palabras separadas por espacios para traducir:"
   s <- readWords
   putStrLn "\nTraducciones:"
   putStrLn (concat (zipWith (\x y -> " ·" ++ x ++ ": " ++ y) s (map (++ "\n") (translate s h))))
   putStrLn ("Longitud media: " ++ show (avgLen s))
  else do
   putStrLn "\nDICCIONARIO:"
   print h
 putStrLn "\n¿Seguir (s), o salir (n)?"
 x <- getLine
 if head x == 's'
  then do
   translations h
   return ()
  else do
   return ()
 
