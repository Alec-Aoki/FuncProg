-- Faça um programa em Haskell que leia dois inteiros não negativos, um em cada linha, e imprima o número de inteiros defeituosos, perfeitos e abundantes entre esses números, um em cada linha.

main = do
    -- Recebendo os dois números inicias
    entrada1 <- getLine
    let n1 = read entrada1

    entrada2 <- getLine
    let n2 = read entrada2

    putStrLn $ show $ divisores $ listaDeLista $ sequencia $ intervalo n1 n2


-- Intervalo: cria uma lista com os números inteiros entre os dois número recebidos
intervalo :: Integer -> Integer -> [Integer]
intervalo a b = [a .. b]

-- listaDeLista: dado uma lista, gera uma lista de listas em que a lista mais interior segue a função f
listaDeLista :: (Integer -> [Integer]) -> [Integer] -> [[Integer]]
listaDeLista _ [] = []
listaDeLista f (x:xs) = f x : listaDeLista f xs

-- Sequencia: dado um inteiro n, produz uma lista de 1 até n-1
sequencia :: Integer -> [Integer]
sequencia x = [1 .. (x-1)] 

-- Divisores: dado uma lista de inteiros e um inteiro, gera uma lista com os divisores desse n que estavam presentes na lista
divisores :: [Integer] -> Integer -> [Integer]
divisores _ 0 = []
divisores [] _ = []
divisores (x:xs) a
    | (mod a x) == 0 = x : divisores xs a 
    | otherwise = divisores xs a

-- Soma: soma todos os inteiros de uma lista
soma :: [Integer] -> Integer
soma [] = 0
soma (x:xs) = x + soma xs
