import Polinomio 

import Test.HUnit

-- Normalizar um polinomio com apenas um numero
testNormalizar1 :: Test
testNormalizar1 = 
    TestCase $ assertEqual "testNormalizar1: O monomio deve ficar igual ao inicial" (Poli [(2,"",[])]) (poli_sort (Poli [(2,"",[])]))
---------

-- Normalizar um polinomio com dois numeros
testNormalizar2 :: Test
testNormalizar2 = 
    TestCase $ assertEqual "testNormalizar2: Os 2 numeros devem somar e apenas ter um monomio" (Poli [(7,"",[])]) (poli_sort (Poli [(2,"",[]),(5,"",[])]))
---------

-- Normalizar polinomio com dois monomios: um numero e uma variavel
testNormalizar3 :: Test
testNormalizar3 =
    TestCase $ assertEqual "testNormalizar3: Deve haver 2 monomios no final com a letra na primeia posição" (Poli [(1,"x",[1]),(5,"",[])]) (poli_sort (Poli [(5,"",[]),(1,"x",[1])]) )
-------



-- Normalizar polinomio com dois monomios com a mesma variavel
testNormalizar4 :: Test
testNormalizar4 = 
    TestCase $ assertEqual "testNormalizar4: Deve somar 2 monomios e resultar apenas um com a mesma variavel" (Poli [(3,"x",[1])]) (poli_sort (Poli [(2,"x",[1]),(1,"x",[1])]))
    
testNormalizar5 :: Test
testNormalizar5 = 
    TestCase $ assertEqual "testNormalizar5: Deve somar 2 monomios e resultar apenas um com a mesma variavel (grau diferente de 1)" (Poli [(3,"x",[2])]) (poli_sort (Poli [(2,"x",[2]),(1,"x",[2])]))

testNormalizar6 :: Test
testNormalizar6 = 
    TestCase $ assertEqual "testNormalizar6: Deve somar 2 monomios e resultar apenas um com a mesma variavel (grau diferente de 1)" (Poli [(3,"xy",[1])]) (poli_sort (Poli [(2,"xy",[1]),(1,"xy",[1])]))

testNormalizar7 :: Test
testNormalizar7 = 
    TestCase $ assertEqual "testNormalizar7: Deve somar 2 monomios e resultar apenas um com a mesma variavel (variaveis trocadas uma com xy e outra com yx)" (Poli [(3,"yx",[1,1])]) (poli_sort (Poli [(2,"xy",[1,1]),(1,"yx",[1,1])]))

testNormalizar8 :: Test
testNormalizar8 = 
    TestCase $ assertEqual "testNormalizar8: Deve haver 2 monomios e resultar em dois estando o que tem maior grau na primeira posição" (Poli [(1,"x",[2]),(2.0,"x",[1])]) (poli_sort (Poli [(2,"x",[1]),(1,"x",[2])]))
-------


-- Testes para Normalizar um polinomio com dois monomios com variaveis diferentes 
testNormalizar9 :: Test
testNormalizar9 =
    TestCase $ assertEqual "testNormalizar9: Deve haver 2 monomios no final com a letra menor na primeia posição" (Poli [(1,"x",[1]),(2,"y",[1])]) (poli_sort (Poli [(2,"y",[1]),(1,"x",[1])]) )

testNormalizar10 :: Test
testNormalizar10 =
    TestCase $ assertEqual "testNormalizar10: Deve haver 2 monomios no final com a letra menor na primeia posição" (Poli [(1,"xy",[1,1]),(2,"yz",[1,1])]) (poli_sort (Poli [(1,"xy",[1,1]),(2,"yz",[1,1])]) )

testNormalizar11 :: Test
testNormalizar11 = 
    TestCase $ assertEqual "testNormalizar11: Deve haver 2 monomios no final com o que tiver o maior maior numero de variaveis na primeia posição" (Poli [(1,"xy",[1,1]),(211,"x",[1])]) (poli_sort (Poli [(211,"x",[1]),(1,"xy",[1,1])]) )

testNormalizar12 :: Test
testNormalizar12 = 
    TestCase $ assertEqual "testNormalizar12: Deve haver 2 monomios no final com o que tiver o maior expoente na primeia posição" (Poli [(1,"y",[2]),(211,"x",[1])]) (poli_sort (Poli [(211,"x",[1]),(1,"y",[2])]) )

testNormalizar13 :: Test
testNormalizar13 = 
    TestCase $ assertEqual "testNormalizar13: Deve haver 2 monomios no final com o que tiver o maior expoente na primeia posição (o menor é do tipo xy)" (Poli [(1,"y",[2]),(211,"xy",[1,1])]) (poli_sort (Poli [(211,"xy",[1,1]),(1,"y",[2])]) )

testNormalizar14 :: Test
testNormalizar14 = 
    TestCase $ assertEqual "testNormalizar14: Deve haver 2 monomios no final com o que tiver o maior expoente na primeia posição (o maior é do tipo xy)" (Poli [(1,"xy",[1,2]),(211,"x",[1])]) (poli_sort (Poli [(211,"x",[1]),(1,"xy",[1,2])]) )

testNormalizar15 :: Test
testNormalizar15 = 
    TestCase $ assertEqual "testNormalizar15: Deve haver 2 monomios no final com o que tiver o maior expoente na primeia posição (ambos do tipo xy)" (Poli [(1,"xy",[1,2]),(211,"yz",[1,1])]) (poli_sort (Poli [(211,"yz",[1,1]),(1,"xy",[1,2])]))
-------


-- Testes para Normalizar um polinomio com vários monómios
testNormalizar16 :: Test
testNormalizar16 =
    TestCase $ assertEqual "testNormalizar16: Primeiro complexo" (Poli [(-7, ['y'], [2]),(0, ['x'], [2]), (3,['y'],[1]),(5 ,['z'],[1])]) (poli_sort (Poli [(0, ['x'], [2]), (2,['y'],[1]),(5 ,['z'],[1]),(1 ,['y'],[1]),(-7, ['y'], [2])]))

testNormalizar17 :: Test
testNormalizar17 =
    TestCase $ assertEqual "testNormalizar17: Segundo complexo" (Poli [(1 ,"yzx",[3,2,4]),(2,['x'],[3]),(2,['y'],[3]),(1 ,"xy",[1,2])]) (poli_sort (Poli [(1 ,"xy",[1,2]),(2,['y'],[3]),(1 ,"yzx",[3,2,4]),(2,['x'],[3])]))
-----

-- Adicionar dois polinomios com dois numeros
testAdicionar1 :: Test
testAdicionar1 =
    TestCase $ assertEqual "testAdicionar1: Deve retornar apenas um numero " (Poli [(5,"",[])]) (add_poly (Poli [(2,"",[])]) (Poli [(3,"",[])]))

testAdicionar2 :: Test
testAdicionar2 =
    TestCase $ assertEqual "testAdicionar2: Deve retornar apenas um numero " (Poli [(1,"",[])]) (add_poly (Poli [(-2,"",[])]) (Poli [(3,"",[])]))
    
------------
-- Adicionar dois polinomios um com um numero e um com uma variavel
testAdicionar3 :: Test
testAdicionar3 =
    TestCase $ assertEqual "testAdicionar3: Deve retornar um monomio numero e outro com uma variavel" (Poli [(1,"x",[1]),(2,"",[])]) (add_poly (Poli [(2,"",[])]) (Poli [(1,"x",[1])]))

testAdicionar4 :: Test
testAdicionar4 =
    TestCase $ assertEqual "testAdicionar4: Deve retornar um monomio numero e outro com uma variavel" (Poli [(1,"x",[1]),(2,"",[])]) (add_poly (Poli [(1,"x",[1])]) (Poli [(2,"",[])]) )

--- Adição de dois polinomios com apenas uma variavel igual em cada um
testAdicionar5 :: Test
testAdicionar5 =
    TestCase $ assertEqual "testAdicionar5: Deve retornar apenas uma variavel com a soma dos coeficientes dos polinomios anteriores" (Poli [(3,"x",[1])]) (add_poly (Poli [(1,"x",[1])]) (Poli [(2,"x",[1])]))

testAdicionar6 :: Test
testAdicionar6 =
    TestCase $ assertEqual "testAdicionar6: Deve retornar apenas uma variavel com a soma dos coeficientes dos polinomios anteriores" (Poli [(3,"xy",[1,1])]) (add_poly (Poli [(1,"xy",[1,1])]) (Poli [(2,"xy",[1,1])]))

testAdicionar7 :: Test
testAdicionar7 =
    TestCase $ assertEqual "testAdicionar7: Deve retornar apenas uma variavel com a soma dos coeficientes dos polinomios anteriores" (Poli [(3,"yx",[1,1])]) (add_poly (Poli [(1,"xy",[1,1])]) (Poli [(2,"yx",[1,1])]))

testAdicionar8 :: Test
testAdicionar8 =
    TestCase $ assertEqual "testAdicionar8: Deve retornar apenas uma variavel com a soma dos coeficientes dos polinomios anteriores" (Poli [(3,"yxz",[1,1,1])]) (add_poly (Poli [(1,"xyz",[1,1,1])]) (Poli [(2,"yxz",[1,1,1])]))

testAdicionar9 :: Test
testAdicionar9 =
    TestCase $ assertEqual "testAdicionar9: Deve retornar apenas uma variavel com a soma dos coeficientes dos polinomios anteriores (grau diferente de 1)" (Poli [(3,"x",[2])]) (add_poly (Poli [(1,"x",[2])]) (Poli [(2,"x",[2])]))

testAdicionar10 :: Test
testAdicionar10 =
    TestCase $ assertEqual "testAdicionar10: Deve retornar apenas uma variavel com a soma dos coeficientes dos polinomios anteriores (grau diferente de 1)" (Poli [(3,"yxz",[1,2,2])]) (add_poly (Poli [(1,"zxy",[2,2,1])]) (Poli [(2,"yxz",[1,2,2])]))
------

-- Soma de dois polinomios com variaveis diferentes
testAdicionar11 :: Test
testAdicionar11 =
    TestCase $ assertEqual "testAdicionar11: Deve retornar dois monomios " (Poli [(2,"x",[1]),(5,"y",[1])]) (add_poly (Poli [(2,"x",[1])]) (Poli [(5,"y",[1])]))

testAdicionar12 :: Test
testAdicionar12 =
    TestCase $ assertEqual "testAdicionar12: Deve retornar dois monomios " (Poli [(5,"y",[2]),(7,"x",[1])]) (add_poly (Poli [(7,"x",[1])]) (Poli [(5,"y",[2])]))

testAdicionar13 :: Test
testAdicionar13 =
    TestCase $ assertEqual "testAdicionar13: Deve retornar dois monomios " (Poli [(5,"xy",[1,2]),(7,"x",[1])]) (add_poly (Poli [(7,"x",[1])]) (Poli [(5,"xy",[1,2])]))
--------

-- Soma casos mais complicados
testAdicionar14 :: Test
testAdicionar14 =
    TestCase $ assertEqual "testAdicionar14: Deve retornar o polinomio correto " (Poli [(2.0,"xy",[1,2]),(0.0,"x",[2]),(-7.0,"y",[2]),(1.0,"x",[1]),(3.0,"y",[1]),(5.0,"z",[1])]) (add_poly (Poli [(0, ['x'], [2]), (2,['y'],[1]),(5 ,['z'],[1]),(1 ,['y'],[1]),(-7, ['y'], [2])]) (Poli [(1 ,"x",[1]),(2,"xy",[1,2])]))

testAdicionar15 :: Test
testAdicionar15 =
    TestCase $ assertEqual "testAdicionar15: Deve retornar o polinomio correto " (Poli [(2.0,"xy",[1,2]),(2.0,"x",[1]),(2.0,"",[])]) (add_poly (Poli [(2,"",[]),(1.0,['x'],[1])]) (Poli [(1 ,"x",[1]),(2,"xy",[1,2])]))

testAdicionar16 :: Test
testAdicionar16 =
    TestCase $ assertEqual "testAdicionar16: Deve retornar o polinomio correto " (Poli [(1.0,"yzx",[3,2,4]),(2.0,"x",[3]),(2.0,"y",[3]),(1.0,"xy",[1,2]),(0.0,"x",[2]),(-7.0,"y",[2]),(3.0,"y",[1]),(5.0,"z",[1])]) (add_poly (Poli [(0, ['x'], [2]), (2,['y'],[1]),(5 ,['z'],[1]),(1 ,['y'],[1]),(-7, ['y'], [2])]) (Poli [(1 ,"xy",[1,2]),(2,['y'],[3]),(1 ,"yzx",[3,2,4]),(2,['x'],[3])]))



-----
-- Multiplicação de dois polinomios com dois numeros
testMultiplicar1 :: Test
testMultiplicar1 = 
    TestCase $ assertEqual "testMultiplicar1: Deve retornar apenas um numero " (Poli [(6,"",[])]) (mult_poli (Poli [(2,"",[])]) (Poli [(3,"",[])]))

-- Multiplicação de um polinomio com um numero por um polinomio com uma variavel
testMultiplicar2 :: Test
testMultiplicar2 = 
    TestCase $ assertEqual "testMultiplicar2: Deve retornar apenas um numero com a multiplicação dos coefiecientes dos polinomios anteriores " (Poli [(2,"x",[1])]) (mult_poli (Poli [(2,"",[])]) (Poli [(1,"x",[1])]))

-- Multiplicação de dois polinomios com apenas uma variavel cada um
testMultiplicar3 :: Test
testMultiplicar3 = 
    TestCase $ assertEqual "testMultiplicar3: Deve retornar apenas uma variavel com a soma dos expoentes dos polinomios anteriores" (Poli [(1,"x",[2])]) (mult_poli (Poli [(1,"x",[1])]) (Poli [(1,"x",[1])]))
    
-- Multiplicação de um polinomio com um numero e uma variavel por um polinomio com uma variavel
testMultiplicar4 :: Test
testMultiplicar4 = 
    TestCase $ assertEqual "testMultiplicar4: Deve um retornar um monomio numero e outro com uma variavel" (Poli [ (4,"",[]),(2,"x",[2])]) (mult_poli (Poli [(2,"",[]), (1,"x",[2])]) (Poli [(2,"",[])]))

-- Multiplicação de dois polinomios com um numero e uma mesma variavel
testMultiplicar5 :: Test
testMultiplicar5 = 
    TestCase $ assertEqual "testMultiplicar5: Deve um retornar um monomio com um numero e tres com uma a mesma variavel x" (Poli [(6,"x",[1]), (4,"",[]), (3, "x", [3]), (2, "x", [2])]) (mult_poli (Poli [(2,"",[]), (1,"x",[2])]) (Poli [(3, "x", [1]),(2,"",[])]))

-- Multiplicação de dois polinomios com um numero e variaveis diferentes
testMultiplicar6 :: Test
testMultiplicar6 = 
    TestCase $ assertEqual "testMultiplicar6: Deve um retornar um monomio numero, dois com a variavel x e um com as variaveis xy" (Poli [(6,"x",[1]), (4,"",[]), (3, "xy", [1,1]), (2, "y", [1])]) (mult_poli (Poli [(2,"",[]), (1,"y",[1])]) (Poli [(3, "x", [1]),(2,"",[])]))

-- Multiplicação de dois polinomios com um numero, variaveis e expoentes diferentes
testMultiplicar7 :: Test
testMultiplicar7 = 
    TestCase $ assertEqual "testMultiplicar7: Deve um retornar um monomio numero, tres com a variavel x, tres com a variavel y e dois com as variaveis xy" (Poli [(2,"y",[2]), (6,"x",[1]), (4,"",[]), (1,"y",[3]), (3,"xy",[1,1]), (2,"y",[1]), (2,"yx",[2,2]), (6,"x",[3]), (4,"x",[2])]) (mult_poli (Poli [(2,"",[]), (1,"y",[1]), (2, "x", [2])]) (Poli [(1, "y", [2]), (3, "x", [1]), (2,"",[])]))

testMultiplicar8 :: Test
testMultiplicar8 = 
    TestCase $ assertEqual "testMultiplicar8: Deve um retornar um monomio com a variavel y, tres com as variaveis xy" (Poli [(2,"y",[2]), (4,"yx",[2,1]), (1,"yx",[4,1]), (2,"yx",[4,2])]) (mult_poli (Poli [(2,"",[]), (1,"xy",[1,2])]) (Poli [(1, "y", [2]), (2,"yx",[2,1])]))

testMultiplicar9 :: Test
testMultiplicar9 = 
    TestCase $ assertEqual "testMultiplicar9: Deve retornar o polinomio correto" (Poli [(2,"xt",[3,3]), (4,"xt",[4,2]), (2,"xt",[1,2]), (1,"xty",[2,3,1]), (2,"xyt",[3,1,2]), (1,"yt",[1,2]), (3,"xt",[3,1]), (6,"x",[4]), (3,"x",[1])]) (mult_poli (Poli [(2,"xt",[1,2]), (1,"yt",[1,2]), (3, "x", [1])]) (Poli [(1, "xt", [2,1]), (2, "x", [3]), (1,"",[])]))

testMultiplicar10 :: Test
testMultiplicar10 = 
    TestCase $ assertEqual "testMultiplicar10: Deve retornar o polinomio correto" (Poli [(1,"xy",[3,2]), (2,"xyt",[2,2,2]), (3,"xyt",[1,1,1])]) (mult_poli (Poli [(1,"xy",[2,1]), (2,"ytx",[1,2,1]), (3, "t", [1])]) (Poli [(1, "xy", [1,1])]))



--Derivar um Polinomio Vazio Por uma letra qualquer
testDerivar1 :: Test
testDerivar1 = 
     TestCase $ assertEqual "testDerivar1: Deve retornar um polinomio vazio " (Poli [(0,"",[])]) (derivada_poli 'x' (Poli []))
-------

--Derivar um Monomio com letra existene
testDerivar2 :: Test
testDerivar2 = 
     TestCase $ assertEqual "testDerivar2: Deve retornar o monomio com o coeficiente multiplicado pelo expoente e reduzir o expoente por 1 " (Poli [(1,"x",[0]),(0,"",[])]) (derivada_poli 'x' (Poli [(1,"x",[1])]))

testDerivar3 :: Test
testDerivar3 = 
     TestCase $ assertEqual "testDerivar3: Deve retornar o monomio com o coeficiente multiplicado pelo expoente e reduzir o expoente por 1 " (Poli [(6,"x",[1]),(0,"",[])]) (derivada_poli 'x' (Poli [(3,"x",[2])]))
---

--Derivar o polinomio só com numeros
testDerivar4 :: Test
testDerivar4 = 
     TestCase $ assertEqual "testDerivar4: Deve retornar um polinomio vazio " (Poli [(0,"",[])]) (derivada_poli 'x' (Poli [(2,"",[]),(6,"",[]),(5,"",[]),(4,"",[])]))
---

--Derivar o polinomio com diferentes graus, todos com a mesma variavel que a escolhida ou só um numero
testDerivar5 :: Test
testDerivar5 = 
     TestCase $ assertEqual "testDerivar5: Deve retornar o polinomios com o coeficientes multiplicados pelo expoente da sua variavel e reduzir o expoente por 1 " (Poli [(9,"x",[2]),(4,"x",[1]),(5,"x",[0]),(0,"",[])]) (derivada_poli 'x' (Poli [(3,"x",[3]),(2,"x",[2]),(5,"x",[1])]))

testDerivar6 :: Test
testDerivar6 = 
     TestCase $ assertEqual "testDerivar6: Deve retornar o polinomios com o coeficientes multiplicados pelo expoente da sua variavel e reduzir o expoente por 1 " (Poli [(9,"x",[2]),(4,"x",[1]),(5,"x",[0]),(0,"",[])]) (derivada_poli 'x' (Poli [(3,"",[]),(3,"x",[3]),(2,"",[]),(2,"x",[2]),(31,"",[]),(5,"x",[1])]))
---------

--Derivar o polinomio com diferentes graus, monomios podem ter ou não a variavel escolhida e com podem ter mais que uma letra, exemplo "xy"
testDerivar7 :: Test
testDerivar7 = 
    TestCase $ assertEqual "testDerivar7: Deve retornar o polinomios com o coeficientes multiplicados pelo expoente da sua variavel e reduzir o expoente por 1 (monomios podem ter duas variaveis ) " (Poli [(9,"xy",[2,1]),(8,"zx",[3,3]),(0,"",[])]) (derivada_poli 'x' (Poli [(3,"",[]),(3,"xy",[3,1]),(3,"z",[3]),(2,"zx",[3,4]),(2,"",[]),(9,"y",[2]),(31,"",[]),(5,"y",[1])]))

testDerivar8 :: Test
testDerivar8 = 
    TestCase $ assertEqual "testDerivar8: Deve retornar o polinomios com o coeficientes multiplicados pelo expoente da sua variavel e reduzir o expoente por 1 (monomios podem ter duas variaveis, desta vez por y ) " (Poli [(3,"xy",[3,0]),(18,"y",[1]),(5,"y",[0]),(0,"",[])]) (derivada_poli 'y' (Poli [(3,"",[]),(3,"xy",[3,1]),(3,"z",[3]),(2,"zx",[3,4]),(2,"",[]),(9,"y",[2]),(31,"",[]),(5,"y",[1])]))

testDerivar9 :: Test
testDerivar9 = 
    TestCase $ assertEqual "testDerivar9: Deve retornar o polinomios com o coeficientes multiplicados pelo expoente da sua variavel e reduzir o expoente por 1 (monomios podem ter três variaveis) " (Poli [(2,"yzx",[3,1,4]),(0,"",[])]) (derivada_poli 'z' (Poli [(1 ,"xy",[1,2]),(2,['y'],[3]),(1 ,"yzx",[3,2,4]),(2,['x'],[3])]))

testDerivar10 :: Test
testDerivar10 = 
    TestCase $ assertEqual "testDerivar10: Deve retornar o polinomios com o coeficientes multiplicados pelo expoente da sua variavel e reduzir o expoente por 1 (monomios podem ter três variaveis) " (Poli [(1 ,"xy",[0,2]),(4,"yzx",[3,2,3]),(6,['x'],[2]),(0,"",[])]) (derivada_poli 'x' (Poli [(1 ,"xy",[1,2]),(2,['y'],[3]),(1 ,"yzx",[3,2,4]),(2,['x'],[3])]))

testDerivar11 :: Test
testDerivar11 = 
    TestCase $ assertEqual "testDerivar11: Deve retornar o polinomios com o coeficientes multiplicados pelo expoente da sua variavel e reduzir o expoente por 1 (monomios podem ter três variaveis) " (Poli [(2,"xy",[1,1]),(6,['y'],[2]),(3,"yzx",[2,2,4]),(0,"",[])]) (derivada_poli 'y' (Poli [(1 ,"xy",[1,2]),(2,['y'],[3]),(1 ,"yzx",[3,2,4]),(2,['x'],[3])]))

testDerivar12 :: Test
testDerivar12 = 
    TestCase $ assertEqual "testDerivar12: Deve retornar o polinomios com o coeficientes multiplicados pelo expoente da sua variavel e reduzir o expoente por 1 (monomios podem ter quatro variaveis) " (Poli [(10 ,"axty",[9,1,9,2]),(20,['t'],[9]),(6,"tui",[2,11,12]),(0,"",[])]) (derivada_poli 't' (Poli [(2,"zxcads",[10,10,123,4,5,6]),(1 ,"axty",[9,1,10,2]),(2,['t'],[10]),(2,"tui",[3,11,12]),(1 ,"yzx",[3,2,4]),(2,"",[]),(2,['x'],[3])]))

testDerivar13 :: Test
testDerivar13 = 
    TestCase $ assertEqual "testDerivar13: Deve retornar o polinomios vazio (monomios podem ter três variaveis) " (Poli [(0,"",[])]) (derivada_poli 'a' (Poli [(1 ,"xy",[1,2]),(2,['y'],[3]),(1 ,"yzx",[3,2,4]),(2,['x'],[3])]))

testes :: IO Counts
testes = runTestTT $ TestList [ testNormalizar1,testNormalizar2,testNormalizar3,testNormalizar4,testNormalizar5,testNormalizar6,testNormalizar5,testNormalizar6,testNormalizar7,testNormalizar8,testNormalizar9,testNormalizar10,testNormalizar11,testNormalizar12,testNormalizar13,testNormalizar14,testNormalizar15,
                                testAdicionar1,testAdicionar2,testAdicionar3,testAdicionar4,testAdicionar5,testAdicionar6,testAdicionar7,testAdicionar8,testAdicionar9,testAdicionar10,testAdicionar11,testAdicionar12,testAdicionar13,testAdicionar14,testAdicionar15,testAdicionar16,
                                testMultiplicar1,testMultiplicar2,testMultiplicar3,testMultiplicar4,testMultiplicar5,testMultiplicar6,testMultiplicar7,
                                testDerivar1,testDerivar2,testDerivar3,testDerivar4,testDerivar5,testDerivar6,testDerivar7,testDerivar8,testDerivar9,testDerivar10,testDerivar11,testDerivar12,testDerivar13 ]
