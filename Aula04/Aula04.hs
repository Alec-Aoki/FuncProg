main = do
    putStrLn "HW"
    putStrLn $ show $ 7 +-=-@ 5
    putStrLn $ show $ mapa dobro a
    putStrLn $ show $ mapa (*2) a
    putStrLn $ show $ pega 2 a

    -- Soma dos quadrados de todos os ímpares de a:
    putStrLn $ show $ soma $ mapa (^2) $ filtra impar a
        -- filtra impar a retorna uma lista dos ímpares de a
        -- mapa (^2) retorna uma lista com os quadrados da lista
        -- soma soma todos os itens da lista

    -- Soma da potência de 2 dos 3 primeiros ímpares de a
    putStrLn $ show $ soma $ mapa (2^) $ pega 3 $ filtra impar a
    putStrLn $
        show $
        soma $
        mapa (2^) $
        pega 3 $
        filtra impar a

    putStrLn $ show $ 5 `pertence` a

    putStrLn $ show $ a ++ [1, 2, 3] -- ++: função concatena

    -- Quicksort
    putStrLn $ show $ quicksort a

a = [3, 5, 2, 1, 9]
dobro x = x * 2

x = 42
y = x + 5

f a b = a^2 + b*2

a +-=-@ b = a^2 + b*2 -- Define a +-=-@ b como a^2 + b*2 (INFIX, vai parecer um operador)

impar x = (mod x 2) == 1 -- mod é prefix

impar2 x = (x `mod` 2) == 1 -- `` transforma em infix

z = (+) x 5 -- () transforma em prefix

add :: Integer -> Integer -> Integer -- add é uma função que recebe um inteiro e retorna uma função que dado um inteiro retorna um inteiro
add a b = a + b

-- Todas as funções seguintes definem a mesma coisa
g x = add 5 x
h = add 5

m x = (+) 5 x
n = (+) 5
p = (+5)

-------------------------------------------------------------------------------------------------------------------------------------------------

-- Mapa: aplica uma operação a todos os itens de uma lista
mapa :: (a -> b) -> [a] -> [b]
mapa _ [] = []
mapa f (x:xs) = f x:mapa f xs

-- OBS: : significa concatenar

-- Filtra: cria uma lista somente com os itens da lista que obedecem o teste
filtra :: (a -> Bool) -> [a] -> [a]
filtra _ [] = []
filtra teste (x:xs)
    |   teste x = x:filtra teste xs
    |   otherwise = filtra teste xs

-- Pega os n primeiro elementos da lista
pega :: Integer -> [a] -> [a]
pega 0 _ = []
pega _ [] = []
pega n (x:xs) = x:pega (n-1) xs

-- Soma todos os elementos da lista
soma :: (Num a) => [a] -> a -- type class Num (tipos que aceitam operações matemáticas)
soma [] = 0
soma (x:xs) = x + soma xs

-- Verifica (como infix) se um elemento pertence a uma lista
pertence :: (Eq a) => a -> [a] -> Bool -- Como comparamos o elemento com a cabeça, não podemos usar um tipo genérico, pois certas coisas não podem ser comparadas (como funções, por exemplo)
_ `pertence` [] =  False
a `pertence` (x:xs)
    |   (a == x) = True
    |   otherwise = a `pertence` xs

-- type
-- type class
-- eq: classe de igualdade, ou seja, pertence a ela todos os tipos em que pode haver comparação
-- https://serokell.io/blog/haskell-typeclasses

-- Quicksort
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (pivot:xs) = menores ++ iguais ++ maiores
    where
        menores = quicksort $ filtra (< pivot) xs
        iguais = pivot:filtra (== pivot) xs
        maiores = quicksort $ filtra (> pivot) xs