-- MENU --
import System.Exit



data Polynomial a b c= Poli [(a,[b],[c])] deriving Show
--   deriving (Eq, Show)
-- a = numero da esquerda
-- b = letras  
-- c = expoente(s), caso cada letra tenha um expoente diferente
poli :: Polynomial Float Char Int
poli = Poli [(0, ['x'], [2]), (2,['y'],[1]),(5 ,['z'],[1]),(1 ,['y'],[1]),(-7, ['y'], [2])]

polivazio :: Polynomial Float Char Int
polivazio = Poli [(1 ,"xy",[0,2]),(2,['y'],[3]),(1 ,"yzx",[3,2,4]),(2,['x'],[3])]


insert_tuple :: (Ord a , Ord b) => (a,b) -> [(a,b)] -> [(a,b)]
insert_tuple (w,x) [] = [(w,x)]
insert_tuple (w,x) ((y,z):yz)
   | w < y = (w,x):(y,z):yz
   |otherwise = (y,z):(insert_tuple (w,x) yz)


-- usar depois de dar zip das variaveis com os seus expoentes
order_variables :: (Ord a, Ord b) => [(a,b)]->[(a,b)]
order_variables l = foldr insert_tuple [] l 


compare_monomio_todo :: (Ord a, Ord b) => [(a,b)] ->[(a,b)]-> Bool
compare_monomio_todo [] _ = True
compare_monomio_todo _ [] = True
compare_monomio_todo ((w,x):wx) ((y,z):yz)
   | w /= y = False
   | x/= z = False
   | otherwise = True && compare_monomio_todo wx yz

compare_monomio_expoente_maior :: (Ord a, Ord b) => [(a,b)] ->[(a,b)]-> Bool
compare_monomio_expoente_maior [] _ = True
compare_monomio_expoente_maior _ [] = True
compare_monomio_expoente_maior ((w,x):wx) ((y,z):yz)
   | w /= y = False
   | x <  z = False
   | otherwise = True && compare_monomio_expoente_maior wx yz

compare_monomio_expoente_menor :: (Ord a, Ord b) => [(a,b)] ->[(a,b)]-> Bool
compare_monomio_expoente_menor [] _ = True
compare_monomio_expoente_menor _ [] = True
compare_monomio_expoente_menor ((w,x):wx) ((y,z):yz)
   | w /= y = False
   | x >  z = False
   | otherwise = True && compare_monomio_expoente_menor wx yz   

{- -- Ns se uso ou não
compare_grau :: (Integer a,Integer b) => [a] -> [b] -> Bool
compare_grau a b = if((sum a) >= (sum b)) then True else False -}

concat_poli :: (Float,[Char],[Int])->Polynomial Float Char Int -> Polynomial Float Char Int
concat_poli (a,b,c) (Poli []) = (Poli [(a,b,c)])
concat_poli (a,b,c) (Poli ((d,e,f):xs)) =  (Poli ((a,b,c):(d,e,f):xs))


-- VER SE AO INSERIR QUANDO AS VARIAVEIS SÂO IGUAIS QUAL CENAS GUARDAR Primeiro if
poli_insert :: (Float,[Char],[Int])->Polynomial Float Char Int -> Polynomial Float Char Int
poli_insert (a,b,c) (Poli []) = (Poli [(a,b,c)])
poli_insert (a,b,c) (Poli ((d,e,f):xs))   
      | compare_monomio_todo (order_variables (zip b c)) (order_variables (zip e f)) == True = (Poli ((d+a,e,f):xs))
      | compare_monomio_expoente_maior  (order_variables (zip b c)) (order_variables (zip e f)) == True = (Poli ((a,b,c):(d,e,f):xs))
      | compare_monomio_expoente_menor  (order_variables (zip b c)) (order_variables (zip e f)) == True = (concat_poli (d,e,f)   (poli_insert (a,b,c) (Poli xs)))
      | maximum c > maximum f = (Poli ((a,b,c):(d,e,f):xs)) 
      | minimum c <= 0 = (concat_poli (d,e,f)   (poli_insert (a,b,c) (Poli xs)))
      | b < e = (Poli ((a,b,c):(d,e,f):xs))
      | otherwise =  (concat_poli (d,e,f)   (poli_insert (a,b,c) (Poli xs)))


poli_sort :: Polynomial Float Char Int -> Polynomial Float Char Int
poli_sort (Poli ((a,b,c):xs) ) = foldr poli_insert (Poli []) (((a,b,c):xs))

print_mon :: [(Char,Int)] -> IO ()
print_mon [] = return ()
print_mon ((x,y):xy) = do
   if(y==1) then 
      putStr   [x]
   else if( y == 0) then return()
   else do 
      putStr $  [x] ++ "^" ++ (show y) 

---  
print_sorted :: Polynomial Float Char Int -> IO ()
print_sorted (Poli []) = return ()
print_sorted (Poli ((d,e,f):xs)) = do
   let monomio = zip e f
   if ( d==0) then
      print_sorted (Poli xs)
   else do
      if(d>0) then do
         putStr $ "+"++ (show d)
         print_mon monomio
         print_sorted (Poli xs)  
      else do
         putStr $ (show d)
         print_mon monomio
         print_sorted (Poli xs) 

-- Verificar se o elemento fornecido pertence à lista
find :: Char -> [Char] -> Bool
find _ [] = False
find n (x:xs)
   | x == n = True
   | otherwise = find n xs

-- Indice do elemento na lista fornecida
index :: [Char] -> Char -> Int
index l n = head [i | (x,i) <- zip l [0 ..], x == n]


sub_expoente :: Int -> [Int] -> [Int]
sub_expoente _ [] = []
sub_expoente n (x:xs)
   | n == 0 = (x-1):xs
   | otherwise = x:sub_expoente (n-1) xs

   
  
derivada_poli :: Char -> Polynomial Float Char Int -> Polynomial Float Char Int
derivada_poli _ (Poli []) = (Poli [])
derivada_poli a (Poli ((d,e,f):xs)) = if( find a e) then (concat_poli (d*(fromIntegral expo),e,new_expo) (derivada_poli a (Poli xs))) else (concat_poli (d,e,f) (derivada_poli a (Poli xs)))
   where index_exp = index e a
         expo = f !! index_exp  
         new_expo = sub_expoente index_exp f 

 


-- Soma dois polinómios
add_poly :: Polynomial Float Char Int -> Polynomial Float Char Int -> Polynomial Float Char Int
add_poly (Poli []) (Poli ((a,b,c):xs) ) = poli_sort (Poli ((a,b,c):xs) )
add_poly (Poli ((d,e,f):df)) (Poli ((a,b,c):xs) ) = add_poly (Poli df) (concat_poli (d,e,f) (Poli ((a,b,c):xs)))


-- Mostra o polinómio normalizado
normalizar_poli :: Polynomial Float Char Int  -> IO()
normalizar_poli (Poli ((d,e,f):xs)) = print_sorted $ poli_sort (Poli ((d,e,f):xs))


menu :: IO ()
menu = do
      putStrLn . unlines $ map concatNums choices
      choice <- getLine
      case validate choice of
         Just n  -> execute . read $ choice
         Nothing -> putStrLn "Please try again"

      menu
   where concatNums (i, (s, _)) = show i ++ ".) " ++ s

validate :: String -> Maybe Int
validate s = isValid (reads s)
   where isValid []            = Nothing
         isValid ((n, _):_) 
               | outOfBounds n = Nothing
               | otherwise     = Just n
         outOfBounds n = (n < 1) || (n > length choices)

choices :: [(Int, (String, IO ()))]
choices = zip [1.. ] [
   ("Normalizar polinomios", norm),
   ("Adicionar polinomios", add),
   ("Multiplicar polinomios", mult),
   ("Calcular a derivada de um polinomio", derv),
   ("Quit", ext)
 ]

execute :: Int -> IO ()
execute n = doExec $ filter (\(i, _) -> i == n) choices
   where doExec ((_, (_,f)):_) = f

removespaces :: String -> String
removespaces [] = []
removespaces (x:xs)
    | x==' ' = removespaces xs
    | otherwise = x:removespaces xs


norm = do
    linha <-getLine
    let semescacos= removespaces linha
    putStr $ removespaces linha
    exitSuccess
add = putStrLn "foo"
mult = putStrLn "foo"
derv = putStrLn "foo"
ext = exitSuccess

-- data Arv a = Vazia | No a (Arv a) (Arv a) deriving Show

-- (0*x^2 + 2*y + 5*z + y + 7*y^2)
  --   7*y^2 + 3*y +5*z +12 
-- strings expoentes                
  -- str1<str2 str1:str2       exp1 > exp2 exp1:exp2




