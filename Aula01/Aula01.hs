main = do -- Monad
  putStrLn "Hello World"

  -- putStrLn x Não funciona pq x não é uma string
  putStrLn (show x) -- Transforma em string

  -- putStrLn show x Não funciona pq vai achar q show é uma string
  putStrLn $ show x -- Sem usar parênteses, o $ primeiro executa o que está na frente
  putStrLn $ show y

  -- putStrLn $ a a não é uma string!!!
  putStrLn $ show c

  putStrLn $ show $ f True
  putStrLn $ show $ f False
  putStrLn $ show $ f $ y > 2 -- putStrLn(show(f(y>2)))

  putStrLn $ show $ g 5
  putStrLn $ show $ g z -- O resultado de g não depende de z, então z não será calculado e compila (Haskell é uma linguagem não-estrita)

  putStrLn $ show $ h 5 -- Compila, a variável usada é o parâmetro

  putStrLn $ show $ k 4

  -- putStrLn $ show o4 vai entrar num loop infinito por causa do z

  putStrLn $ show $ f1 o1
  putStrLn $ show $ f1 o4 -- Compila pois não usa o z

  putStrLn $ show $ 5:o3 -- Coloca um item no início da lista, não é possível colocar  no final

  putStrLn $ show $ s o3

  putStrLn $ show $ head o3 -- Mostra a cabeça de uma lista
  putStrLn $ show $ tail o3 -- Mostra a cauda (tudo menos a cabeça) de uma lista, sem precisar calcular a cabeça!!

-- O tipo de uma variável númerica não é definido até o seu uso
y = x + 1 -- A ordem da DEFINIÇÃO não importa
x = 5 -- Define x como 5
-- x = x + 1 Não compila, duas declarações para x

z = z + 1 -- Compila, não significa "calcule z", significa "defina z"; enquanto não usar o z, não vai entrar em um loop infinito
w = z + 1 -- Compila pois não usa o valorr de z. Caso eu quisesse mostrar o valor de w, entraria no loop

-- Tipo booleanos
a = True
b = False
c = 5 > y

-- Funções
f True = 10
f False = 100

h z = z + 1 -- z se refere ao parâmetro!

-- i e j são funções equivalentes
i x True = x - 1
i x False = x + 1

j x b = if b
          then x - 1
          else x + 1

-- Fatorial de x
k 0 = 1
k x = x * k (x - 1)

-- Função constante e polimórfica (trabalha com qualquer tipo)
g t = 57

-- Listas
o1 = [] -- Lista vazia, de qualquer coisa
o2 = [5] -- Lista de números
o3 = [6, 42, x, i 10 True] -- Se colocasse só o i sem o argumento não funcionaria
-- Não é possível listas com tipos diferentes!!!
o4 = [True, False, a, z < 100] -- Lista de booleanos, funciona pois não calculou o z

-- Funções com listas
f1 [] = 0
-- f1 x = 1
f1 (x:xs) = 1 + f1 xs -- Consegue contar a quantidade de elementos na lista(?), sem calcular seus elementos
-- O operador : é usado para concatenar listas

-- Somador de elementos da lista
s [] = 0
s (x:xs) = x + s xs