-- MENU --
import System.Exit
import Data.List
import Data.List.Split
import Data.Char
data Polynomial a b c= Poli [(a,[b],[c])] deriving Show
--   deriving (Eq, Show)
-- a = numero da esquerda
-- b = letras  
-- c = expoente(s), caso cada letra tenha um expoente diferente
poli :: Polynomial Float Char Int
poli = Poli [(0, ['x'], [2]), (2,['y'],[1]),(5 ,['z'],[1]),(1 ,['y'],[1]),(-7, ['y'], [2])]

polivazio :: Polynomial Float Char Int
polivazio = Poli [(1 ,"xy",[1,2]),(2,['y'],[3]),(1 ,"yzx",[3,2,4]),(2,['x'],[3])]

poli2 :: Polynomial Float Char Int
poli2 = Poli [(1 ,"xx",[1,1]),(2,['y'],[3])]


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
      | maximum f > maximum c = (concat_poli (d,e,f)   (poli_insert (a,b,c) (Poli xs)))
      | minimum c <= 0 = (concat_poli (d,e,f)   (poli_insert (a,b,c) (Poli xs)))
      | length b < length e = (concat_poli (d,e,f)   (poli_insert (a,b,c) (Poli xs)))
      | b < e = (Poli ((a,b,c):(d,e,f):xs))
      | otherwise =  (concat_poli (d,e,f)   (poli_insert (a,b,c) (Poli xs)))


poli_sort :: Polynomial Float Char Int -> Polynomial Float Char Int
poli_sort (Poli ((a,b,c):xs) ) = foldr poli_insert (Poli []) (((a,b,c):xs))

print_mon :: [(Char,Int)] -> IO ()
print_mon [] = return ()
print_mon ((x,y):xy) = do
   if(y==1) then(
      do
         putStr  ([x])
         print_mon xy
      ) 
   else if( y == 0) then return()
   else(do 
         putStr ( [x] ++ "^" ++ (show y) )
         print_mon xy
         ) 
      


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
find_elem :: Char -> [Char] -> Bool
find_elem _ [] = False
find_elem n (x:xs)
   | x == n = True
   | otherwise = find_elem n xs

-- Indice do elemento na lista fornecida
index :: [Char] -> Char -> Int
index l n = head [i | (x,i) <- zip l [0 ..], x == n]


sub_expoente :: Int -> [Int] -> [Int]
sub_expoente _ [] = []
sub_expoente n (x:xs)
   | n == 0 = (x-1):xs
   | otherwise = x:sub_expoente (n-1) xs


-- adicionar um valor dado a um expoente da lista fornecida
add_expoente :: Int -> Int -> [Int] -> [Int]
add_expoente _ _ [] = []
add_expoente n inc (x:xs)
   | n == 0 = (x+inc):xs
   | otherwise = x:add_expoente (n-1) inc xs


--- QUando não existe letra meter tudo a 0 ou não existe
-- Em vez de mudar apenas retirei do polinomio
derivada_poli :: Char -> Polynomial Float Char Int -> Polynomial Float Char Int
derivada_poli _ (Poli []) = (Poli [])
derivada_poli a (Poli ((d,e,f):xs)) = if( find_elem a e) then (concat_poli (d*(fromIntegral expo),e,new_expo) (derivada_poli a (Poli xs))) else ( (derivada_poli a (Poli xs)))
   where index_exp = index e a
         expo = f !! index_exp  
         new_expo = sub_expoente index_exp f 

num1 :: Polynomial Float Char Int
num1 = Poli [(1 ,"x",[1]),(-2,"xy",[3,1])]
num2 :: Polynomial Float Char Int
num2 = Poli [(1 ,"y",[2]),(3,"x",[2])]
mon1 :: (Float, [Char], [Int])
mon1 = (2 ,"abt",[1, 1, 1])
mon2 :: (Float, [Char], [Int])
mon2 = (3,"xyt",[1, 2, 2])
 
-- multiplica dois polinomios
mult_poli :: Polynomial Float Char Int -> Polynomial Float Char Int -> Polynomial Float Char Int
mult_poli (Poli a) (Poli b) = Poli [ mult_mon x y | x <- a, y <- b]

-- multiplica dois monomios
mult_mon :: (Float, [Char], [Int]) -> (Float, [Char], [Int]) -> (Float, [Char], [Int])
mult_mon (a, [], []) (d, e, f) = (a*d, e, f)
mult_mon (a, b, c) (d, [], []) = (a*d, b, c)
mult_mon (a, var:vars, exp:exps) (d,e,f) = if (a==0 || d==0) then (0, [], [])
                           else (
                              if (find_elem var e)
                                 then mult_mon (a, vars, exps) (d, e, new_f)
                              else
                                 mult_mon (a, vars, exps) (d, e++[var], f++[exp])
                              )
                           where index_exp = index e var
                                 inc = getElem index_exp (exp:exps)
                                 new_f = add_expoente index_exp inc f


getElem :: Int -> [Int] -> Int
getElem _ [] = 0
getElem idx list = if (l == []) then 0 else head l where l = [el | (el, i)<-(zip list [0..]), i==idx]


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

separateMonom :: String -> String
separateMonom [] = []
separateMonom [l] =[l]
separateMonom (x:xs)
   | x=='+' || x=='-' = ' ':x:separateMonom xs
   |otherwise = x:separateMonom xs


--- Acho que não é usado
splitOnAnyOf :: Eq a => [[a]] -> [a] -> [[a]]
splitOnAnyOf ds xs = foldl' (\ys d -> ys >>= splitOn d) [xs] ds

print_monomio_sperads :: [String] ->IO()
print_monomio_sperads [] = return ()
print_monomio_sperads (x:xs) = do
      putStrLn x 
      print_monomio_sperads xs




-- para usar quando já tiverem separados em monomios
addCoeficient :: String -> String
addCoeficient [] = []
addCoeficient (x:xs)
   | x=='-' && (isLetter (head(xs))) = "-1"++ xs  
   | x=='+' && (isLetter (head(xs))) = "+1"++ xs 
   | isLetter x == True = "1"++(x:xs)
   |otherwise = x:xs

-- para usar quando já tiverem separados em monomios
addMissingExponent :: String -> String
addMissingExponent [] = []
addMissingExponent (x:xs)
   | isLetter x && xs==[] = [x]++ "^1"
   | isLetter x && (head(xs)) /= '^' = [x]++ "^1" ++ addMissingExponent xs
   | otherwise = x:addMissingExponent xs

-- para usar quando já tiverem separados em monomios
getCoeficient :: String -> Float
getCoeficient [] = 0
getCoeficient l = read (takeWhile (\x -> (isNumber x) || (x=='+') || (x=='-') || (x=='.') ) l) :: Float

-- para usar quando já tiverem separados em monomios
getVars :: String -> String
getVars [] = []
getVars  (x:xs) 
   |isLetter x = x:getVars xs
   | otherwise = getVars xs


 -- para usar quando já tiverem separados em monomios
getExpoents :: String -> [Int]
getExpoents  [] = []
getExpoents (x:xs)
   | x=='^' = (read (takeWhile (isNumber) xs) ::Int):getExpoents xs
   | otherwise = getExpoents xs
 

 

-- map trun to tuple [strig]
getTuplo:: [String]-> Polynomial Float Char Int
getTuplo l = Poli [(getCoeficient u , getVars u , getExpoents u) | u<-l , u/=[]]
      
parse :: String -> String
parse l = addCoeficient (addMissingExponent l)

--  string -> [strings] -> addCoeficient na lista toda -> 
start= do
      putStrLn "Insira a expressão"
      expression <- getLine
      putStrLn ("Expressão: " ++  (separateMonom(filter (/=' ') expression)))
      let monomios_separados = map parse (splitOn [' '] (separateMonom(filter (/=' ') expression))) 
      let polia= getTuplo monomios_separados
     -- show polia
      print_monomio_sperads monomios_separados


