-- MENU --
import System.Exit



data Polynomial a b c= Poli [(a,[b],[c])] deriving Show
--   deriving (Eq, Show)
-- a = numero da esquerda
-- b = letras  
-- c = expoente(s), caso cada letra tenha um expoente diferente
compare_monomio :: (Ord a, Ord b) => [(a,b)] ->[(a,b)]-> Bool
compare_monomio [] _ = True
compare_monomio _ [] = True
compare_monomio ((w,x):wx) ((y,z):yz)
   | w /= y = False
   | x/= z = False
   | otherwise = True && compare_monomio wx yz

insert_tuple :: (Ord a , Ord b) => (a,b) -> [(a,b)] -> [(a,b)]
insert_tuple (w,x) [] = [(w,x)]
insert_tuple (w,x) ((y,z):yz)
   | w < y = (w,x):(y,z):yz
   |otherwise = (y,z):(insert_tuple (w,x) yz)

order :: (Ord a, Ord b) => [(a,b)]->[(a,b)]
order l = foldr insert_tuple [] l 


poli :: Polynomial Float Char Int
poli = Poli [(2, ['x'], [2]), (3,['d'],[5])]

polivazio :: Polynomial Float Char Int
polivazio = Poli []

poli_insert :: (Float,[Char],[Int])->Polynomial Float Char Int -> Polynomial Float Char Int
poli_insert (a,b,c) (Poli []) = (Poli [])
poli_insert (a,b,c) (Poli ((d,e,f):xs))
      | c > f = Poli ((a,b,c):(d,e,f):xs)
      | head b == 'd' = (Poli ((a,b,c):xs))
      | (head c) == 1 = (Poli ((a,b,c):xs))
      | otherwise = (Poli ((a,b,c):xs))



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




