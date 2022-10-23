# Programação Funcional e em Lógica Trabalho Prático 1


# Representação do polinómio

- Para representar um polinómio usou-se um tipo Data que contém uma lista de tuplos. Cada tuplo representa um monómio do polinómio inicial. Este tuplo é constituído por três valores: o primeiro é o coeficiente do monómio; o segundo é uma lista de caracteres com as variáveis do mesmo monómio; e o último valor é uma lista de inteiros que representam os expoentes das variáveis da lista anterior (estes expoentes estão nas posições correspondentes às posições das variáveis). Esta representação permite assim uma boa leitura do polinómio no código, sem complicar a implementação das funções em que é usado.

# Funcionalidades


## Normalização de polinómios
	
Para normalizar um polinómio decidimos organizar um polinómio de acordo com as regras da normalização. Para isso decidimos usar o método de inserção usando foldr e uma função de insert, no nosso caso poli_insert, baseado no exercício 3.4 da folha de exercícios 3.<br>
Nesta função vamos comparar o monómio que estamos a querer inserir com os monómios que já se encontram no polinômio. Dependendo das condições o monômio que estamos a inserir pode ficar à esquerda do primeiro monómio que tenha menor grau, ou seja alfabeticamente menor que ele, ou caso tenho as mesmas variáveis e os mesmos expoentes, apenas adiciona o seu coeficiente ao do monómio que já se encontra no polinómio.


## Adição de polinómio

Para adicionar dois polinómios, apenas se construiu um novo polinómio onde a sua lista de monómios é a concatenação das listas dos monómios dos dois polinómios a adicionar. Após esta operação, basta realizar a normalização desse polinómio resultante (usando a funcionalidade já implementada da normalização de polinómios).


## Multiplicação de polinómios

Inicialmente, usou-se uma lista de compreensão para aceder a todos os monómios de cada um dos polinómios a multiplicar, permitindo assim obter cada um dos tuplos de monômios que resultam da aplicação da propriedade distributiva da multiplicação. O produto entre os monómios destes tuplos é calculado por uma função auxiliar mult_mon, que retorna o resultado no mesmo formato dos monómios.<br>
Os produtos anteriores são recolhidos na função mult_poli que constrói a lista de monómios que constitui o polinômio resultante da multiplicação dos polinómios iniciais.


## Derivação de polinómios
	
Para derivar, primeiro vemos se a variável que escolhemos existe no primeiro monómio do polinómio, se existir multiplicamos o coeficiente pelo expoente dessa variável, reduzimos o expoente dessa variável por um, e no final concatena-mo-lo ao resultado da derivada dos restantes monómios. <br>
Se não existir, descartamos esse monómio e percorremos o resto do polinómio.


# Exemplos de utilização das funcionalidades do programa

## Exemplo do parse de uma string para um polinómio:

![Parse_string]("parsePFL.PNG")

Neste exemplo é possível visualizar como o nosso código vai guardar uma expressão dada num polinómio.


## Exemplo de normalização de um polinómio:

![Normalizacao]("imagens/normalizacao.PNG")

Após carregar o ficheiro com o código do programa, executa-se a função menu que permite ao utilizador selecionar uma das opções apresentadas. Neste exemplo selecionou-se a opção 1 e, de seguida, inseriu-se a expressão do polinómio que se pretende normalizar. Por fim, é retornada a expressão inserida e a expressão do polinómio normalizado.


## Exemplo de adição de dois polinómios:

![Adicao]("imagens/adicao.PNG")

	
Ao selecionar a opção 2 para adicionar polinómios, devemos inserir o primeiro polinómio e de seguida o segundo. O programa retorna em ambos os casos a expressão do polinómio inserido. No fim da execução desta opção, este retorna o polinómio resultante da soma dos anteriores.

## Exemplo de multiplicação de dois polinómios:

![Multiplicacao]("imagens/multiplicacao.PNG")

Depois de selecionar a opção 3 para multiplicação de polinómios, o procedimento de inserção dos polinómios é similar ao utilizado na adição. O resultado final é o produto dos dois polinómios introduzidos anteriormente.

## Exemplo da derivada de um polinómio:

![Derivacao]("imagens/derivacao.PNG")

Após selecionar a opção 4 para derivar um polinómio, é necessário introduzir a expressão do polinómio a derivar e depois a variável segundo a qual se pretende deriva. Por último, o programa retorna a expressão da derivada desse polinómio.
