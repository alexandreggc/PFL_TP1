-- MENU --
import System.Exit



data Polynomial a b c= Poli [(a,[b],[c])] deriving Show
--   deriving (Eq, Show)
-- a = numero da esquerda
-- b = letras  
-- c = expoente(s), caso cada letra tenha um expoente diferente

poli :: Polynomial Int Char Int
poli = Poli [(2, ['x'], [2]), (3,['d'],[5])]

poli2 :: Polynomial Int Char Int -> Bool
poli2 (Poli ((a,b,c):xs))
    | a == 1 = True
    |  head b == 'd' = True
    | (head c) == 1 = True
    | otherwise = poli2 (Poli xs)

    
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
    putStr semescacos
    exitSuccess
add = putStrLn "foo"
mult = putStrLn "foo"
derv = putStrLn "foo"
ext = exitSuccess

-- data Arv a = Vazia | No a (Arv a) (Arv a) deriving Show

-- (0*x^2 + 2*y + 5*z + y + 7*y^2)
     7*y^2 + 3*y +5*z +12 
-- strings expoentes                
  -- str1<str2 str1:str2       exp1 > exp2 exp1:exp2




-- Funcoes de arvore aula4 --

data Arv a = Vazia | No a (Arv a) (Arv a) deriving Show

type Pair a b = (a, b)


-- 4.1 --
sumArv :: (Num a) => Arv a -> a
sumArv Vazia = 0
sumArv (No x l r) = x + (sumArv l) + (sumArv r)

myArv :: Arv Int
myArv = (No 3(No 2(No 1 Vazia Vazia) Vazia) (No 4 Vazia Vazia))

-- 4.4 --
mapArv :: (a -> b) -> Arv a -> Arv b 
mapArv _ Vazia = Vazia
mapArv f (No x l r) = No (f x) (mapArv f l) (mapArv f r)

f :: (Num a) => a -> a
f a = a+a


-- 3.4 --
insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) =   if x <= y
                    then x:y:ys
                    else y : insert x ys

isort :: Ord a => [a] -> [a]
isort l = foldr insert 0 l
