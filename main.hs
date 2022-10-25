-- MENU --
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Polinomio where

import System.Exit
import Data.List
import Data.List.Split
import Data.Char

import PFL2022TP1Tests
import Data.String (IsString)

data Polynomial a b c= Poli [(a,[b],[c])] deriving (Eq,Show)
-- (a,[b],[c]) -> Representa um Monomio
-- a = Coeficientes
-- b = variaveis  
-- c = expoentes de cada variavel

-- Polinomios para testes
poli1 :: Polynomial Float Char Int
poli1 = Poli [(0, ['x'], [2]), (2,['y'],[1]),(5 ,['z'],[1]),(1 ,['y'],[1]),(-7, ['y'], [2])]

poli2 :: Polynomial Float Char Int
poli2 = Poli [(1 ,"xy",[1,2]),(2,['y'],[3]),(1 ,"yzx",[3,2,4]),(2,['x'],[3])]

poli3 :: Polynomial Float Char Int
poli3 = Poli [(1 ,"x",[1]),(2,"xy",[1,2])]

poli4 :: Polynomial Float Char Int
poli4 = Poli [(1.0,['x'],[1]),(2,"",[])]

num1 :: Polynomial Float Char Int
num1 = Poli [(1 ,"xt",[1, 1]),(-2,"xy",[3,1])]

num2 :: Polynomial Float Char Int
num2 = Poli [(1 ,"yt",[2, 1]),(3,"x",[2])]

mon1 :: (Float, [Char], [Int])
mon1 = (2 ,"xyt",[1, 1, 1])

mon2 :: (Float, [Char], [Int])
mon2 = (3,"xy",[1, 1])
------------

-- Insere um tuplo de forma alfabeticca
insert_tuple :: (Ord a , Ord b) => (a,b) -> [(a,b)] -> [(a,b)]
insert_tuple (w,x) [] = [(w,x)]
insert_tuple (w,x) ((y,z):yz)
   | w < y = (w,x):(y,z):yz
   |otherwise = (y,z):(insert_tuple (w,x) yz)

-- Ordena a lista de tuplos alfabeticamente
order_variables :: (Ord a, Ord b) => [(a,b)]->[(a,b)]
order_variables l = foldr insert_tuple [] l 


-- Concatena um monomio a um polinomio
concat_poli :: (Float,[Char],[Int])->Polynomial Float Char Int -> Polynomial Float Char Int
concat_poli (a,b,c) (Poli []) = (Poli [(a,b,c)])
concat_poli (a,b,c) (Poli ((d,e,f):xs)) =  (Poli ((a,b,c):(d,e,f):xs))


-- Insere um monomio num polinomio de acordo com as regras de normalização
poli_insert :: (Float,[Char],[Int])->Polynomial Float Char Int -> Polynomial Float Char Int
poli_insert (a,b,c) (Poli []) = (Poli [(a,b,c)])
poli_insert (a,b,c) (Poli ((d,e,f):xs))
      | null(b) && null(e) = (Poli ((d+a,e,f):xs))
      | not (null (b)) && (null(e)) = (Poli ((a,b,c):(d,e,f):xs)) 
      | null (b) && (not (null(e)))=  (concat_poli (d,e,f)   (poli_insert (a,b,c) (Poli xs)))
      | (not (null(b))) && null(e) = (Poli ((d+a,e,f):xs))
      | (order_variables (zip b c)) == (order_variables (zip e f)) = (Poli ((d+a,e,f):xs))
      | maximum c > maximum f = (Poli ((a,b,c):(d,e,f):xs)) 
      | maximum f > maximum c = (concat_poli (d,e,f)   (poli_insert (a,b,c) (Poli xs)))
      | minimum c <= 0 = (concat_poli (d,e,f)   (poli_insert (a,b,c) (Poli xs)))
      | length b < length e = (concat_poli (d,e,f)   (poli_insert (a,b,c) (Poli xs)))
      | b < e = (Poli ((a,b,c):(d,e,f):xs))
      | otherwise =  (concat_poli (d,e,f)   (poli_insert (a,b,c) (Poli xs)))

-- Ordena um polinomio de acordo com as regras de normalização de um polinomio
poli_sort :: Polynomial Float Char Int -> Polynomial Float Char Int
poli_sort (Poli ((a,b,c):xs) ) = foldr poli_insert (Poli []) (((a,b,c):xs))


-- Dá print às variaveis e aos seus expoentes de um monomio
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
      
---Dá print a um Polinomio
print_sorted :: Polynomial Float Char Int -> IO ()
print_sorted (Poli []) = putStrLn ""
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

-- Subtrai um ao expoente indicado
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


-- Deriva a um polinomio, caso o monomio não contenha a letra indicada para derivar ele é retirado do polinomio
derivada_poli :: Char -> Polynomial Float Char Int -> Polynomial Float Char Int
derivada_poli _ (Poli []) = Poli [(0,"",[])] 
derivada_poli a (Poli ((d,e,f):xs)) = if( find_elem a e) then (concat_poli (d*(fromIntegral expo),e,new_expo) (derivada_poli a (Poli xs))) else ( (derivada_poli a (Poli xs)))
   where index_exp = index e a
         expo = f !! index_exp  
         new_expo = sub_expoente index_exp f 


 
-- multiplica dois polinomios
mult_poli :: Polynomial Float Char Int -> Polynomial Float Char Int -> Polynomial Float Char Int
mult_poli (Poli a) (Poli b) = Poli [ mult_mon x y | x <- a, y <- b]

-- multiplica dois monomios
mult_mon :: (Float, [Char], [Int]) -> (Float, [Char], [Int]) -> (Float, [Char], [Int])
mult_mon (a, [], []) (d, e, f) = (a*d, e, f)
mult_mon (a, b, c) (d, [], []) = (a*d, b, c)
mult_mon (a, var:vars, exp:exps) (d,e,f) = if (a==0 || d==0) then (0, [], [])
                           else (
                              if (find_elem var e) then
                                 mult_mon (a, vars, exps) (d, e, new_f)
                              else
                                 mult_mon (a, vars, exps) (d, e++[var], f++[exp])
                              )
                           where index_exp = index e var
                                 new_f = add_expoente index_exp exp f


-- Soma dois polinómios
add_poly :: Polynomial Float Char Int -> Polynomial Float Char Int -> Polynomial Float Char Int
add_poly (Poli []) (Poli ((a,b,c):xs) ) = poli_sort (Poli ((a,b,c):xs) )
add_poly (Poli ((d,e,f):df)) (Poli ((a,b,c):xs) ) = add_poly (Poli df) (concat_poli (d,e,f) (Poli ((a,b,c):xs)))


-- Mostra o polinómio normalizado
normalizar_poli :: Polynomial Float Char Int  -> IO()
normalizar_poli (Poli ((d,e,f):xs)) = print_sorted $ poli_sort (Poli ((d,e,f):xs))

-- Usado para separar os monomios, coloca um espaço entre eles
separateMonom :: String -> String
separateMonom [] = []
separateMonom [l] =[l]
separateMonom (x:xs)
   | x=='+' || x=='-' = ' ':x:separateMonom xs
   |otherwise = x:separateMonom xs


-- Dá print dos monomios conseguidos da string lida, usado só para debugging
print_monomio_sperads :: [String] ->IO()
print_monomio_sperads [] = return ()
print_monomio_sperads (x:xs) = do
      putStrLn x 
      print_monomio_sperads xs


-- Adiciona um "1" caso seja apenas uma variavel, exemplo "x"->"1x" ou um "-1" no cado de "-x"->"-1x"
addCoeficient :: String -> String
addCoeficient [] = []
addCoeficient (x:xs)
   | x=='-' && (isLetter (head(xs))) = "-1"++ xs  
   | x=='+' && (isLetter (head(xs))) = "+1"++ xs 
   | isLetter x == True = "1"++(x:xs)
   |otherwise = x:xs

-- Adiciona "^1" às variaveis que não têm expoente
addMissingExponent :: String -> String
addMissingExponent [] = []
addMissingExponent (x:xs)
   | isLetter x && xs==[] = [x]++ "^1"
   | isLetter x && (head(xs)) /= '^' = [x]++ "^1" ++ addMissingExponent xs
   | otherwise = x:addMissingExponent xs

-- Consegue o coefience de um monomio
getCoeficient :: String -> Float
getCoeficient [] = 0
getCoeficient l = read ( (if((head number) == '+') then tail number else number)) :: Float
   where number=takeWhile (\x -> (isNumber x) || (x=='+') || (x=='-') || (x=='.') ) l


-- Consegue apenas as variaveis de um monomio
getVars :: String -> String
getVars [] = []
getVars  (x:xs) 
   |isLetter x = x:getVars xs
   | otherwise = getVars xs


 --Consegue uma lista com os expoentes de cada variavel do monomio
getExpoents :: String -> [Int]
getExpoents  [] = []
getExpoents (x:xs)
   | x=='^' = (read (takeWhile (isNumber) xs) ::Int):getExpoents xs
   | otherwise = getExpoents xs
 

-- transforma uma lista de monomios num polinomio
getTuplo:: [String]-> Polynomial Float Char Int
getTuplo l = Poli [(getCoeficient u , getVars u, getExpoents u) | u<-l , u/=[]]

-- adiciona coefieciente, caso não exista, e expoentes às variaveis do monomio que não tenham   
parse :: String -> String
parse l = addCoeficient (addMissingExponent l)


-- menu
menu :: IO()
menu = do
      putStrLn "\n1) Normalizar polinomio"
      putStrLn "2) Adicionar polinomios"
      putStrLn "3) Multiplicar polinomios"
      putStrLn "4) Derivar polinomio"
      putStrLn "0) Sair"
      putStrLn "Insira a opção: "
      opc <- getChar
      if (opc == '1') then
         do 
            putStrLn "\nInsira a expressão inicial:"
            expression <- getLine
            putStrLn ("Expressão: " ++  (filter (/=' ') expression))
            let polinomio =getTuplo (map parse (splitOn [' '] (separateMonom(filter (/=' ') expression))) )
            putStr ("Polinomio normalizado: ")
            normalizar_poli polinomio 
            menu
      else if (opc == '2') then
         do
            putStrLn "\nInsira o primeiro polinomio:"
            expression1 <- getLine
            putStrLn ("Primeiro polinomio: " ++  (filter (/=' ') expression1))
            putStrLn "\nInsira o segundo polinomio:"
            expression2 <-getLine
            putStrLn ("Segundo polinomio: " ++  (filter (/=' ') expression2))
            let pol1 = getTuplo (map parse (splitOn [' '] (separateMonom(filter (/=' ') expression1))) )
            let pol2 = getTuplo (map parse (splitOn [' '] (separateMonom(filter (/=' ') expression2))) )
            putStr ("\nResultado:")
            normalizar_poli (add_poly pol1 pol2)
            menu
      else if (opc == '3') then
         do
            putStrLn "\nInsira o primeiro polinomio:"
            expression1 <- getLine
            putStrLn ("Primeiro polinomio: " ++  (filter (/=' ') expression1))
            putStrLn "\nInsira o segundo polinomio:"
            expression2 <-getLine
            putStrLn ("Segundo polinomio: " ++  (filter (/=' ') expression2))
            let pol1 = getTuplo (map parse (splitOn [' '] (separateMonom(filter (/=' ') expression1))) )
            let pol2 = getTuplo (map parse (splitOn [' '] (separateMonom(filter (/=' ') expression2))) )
            putStr ("\nResultado:")
            normalizar_poli (mult_poli pol1 pol2)
            menu
      else if (opc == '4') then
         do
            putStrLn "\nInsira a expressão a derivar:"
            expression <- getLine
            putStrLn ("Expressão: " ++  (filter (/=' ') expression))
            putStrLn "\nInsira a variavel:"
            var <- getChar
            let polinomio = getTuplo (map parse (splitOn [' '] (separateMonom(filter (/=' ') expression))) )
            if (isNumber var) then
               do
                  putStrLn ("\nVariavel invalida")
                  menu
            else 
               do 
                  putStr ("\nPolinomio derivado: ")
                  normalizar_poli (derivada_poli var polinomio) 
                  menu
      else if (opc == '0') then
         do
            putStrLn "\nPrograma terminado!"
      else menu 
         
output :: String -> String -> IO ()
output str1 str2 = normalizar_poli ( add_poly (getTuplo (map parse (splitOn [' '] (separateMonom(filter (/=' ') str1))))) (getTuplo (map parse (splitOn [' '] (separateMonom(filter (/=' ') str2))))))

output2 :: Char -> String -> IO ()
output2 c str = normalizar_poli(derivada_poli c (getTuplo (map parse (splitOn [' '] (separateMonom(filter (/=' ') str))))))

output_mult :: String -> String -> IO ()
output_mult str1 str2 = normalizar_poli ( mult_poli (getTuplo (map parse (splitOn [' '] (separateMonom(filter (/=' ') str1))))) (getTuplo (map parse (splitOn [' '] (separateMonom(filter (/=' ') str2))))))


output_mult1 :: String -> String -> String
output_mult1 str1 str2 = show ( mult_poli (getTuplo (map parse (splitOn [' '] (separateMonom(filter (/=' ') str1))))) (getTuplo (map parse (splitOn [' '] (separateMonom(filter (/=' ') str2))))))

