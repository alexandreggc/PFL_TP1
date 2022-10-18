-- MENU --
import System.Exit



data Polynomial a b c= Poli [(a,[b],[c])] deriving Show
--   deriving (Eq, Show)
-- a = numero da esquerda
-- b = letras  
-- c = expoente(s), caso cada letra tenha um expoente diferente
poli :: Polynomial Float Char Int
poli = Poli [(2, "xyz", [2,3,3]), (3,"yzx",[3,3,3]),(1 ,['x'],[5]),(5 ,['x'],[5])]

polivazio :: Polynomial Float Char Int
polivazio = Poli []


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

-- VER SE AO INSERIR QUANDO AS VARIAVEIS SÂO IGUAIS QUAL CENAS GUARDAR Primeiro if
poli_insert :: (Float,[Char],[Int])->Polynomial Float Char Int -> Polynomial Float Char Int
poli_insert (a,b,c) (Poli []) = (Poli [(a,b,c)])
poli_insert (a,b,c) (Poli ((d,e,f):xs))   
      | compare_monomio_todo (order_variables (zip b c)) (order_variables (zip e f)) == True = (Poli ((d+a,e,f):xs))
      | compare_monomio_expoente_maior  (order_variables (zip b c)) (order_variables (zip e f)) == True = (Poli ((a,b,c):(d,e,f):xs))
      | compare_monomio_expoente_menor  (order_variables (zip b c)) (order_variables (zip e f)) == True = (Poli ((d,e,f):(a,b,c):xs))
      | maximum c > maximum f = (Poli ((a,b,c):(d,e,f):xs)) 
      | b < e = (Poli ((a,b,c):(d,e,f):xs))
      | otherwise = (Poli ((d,e,f):(a,b,c):xs))

  

poli_sort :: Polynomial Float Char Int -> Polynomial Float Char Int
poli_sort (Poli ((a,b,c):xs) ) = foldr poli_insert (Poli []) (((a,b,c):xs))

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




