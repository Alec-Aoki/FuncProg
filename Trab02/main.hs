-- Faça um programa em Haskell que leia dois inteiros não negativos, um em cada linha, e imprima o número de inteiros defeituosos, perfeitos e abundantes entre esses números, um em cada linha.

main = do
    -- Recebendo os dois números inicias
    entrada1 <- getLine
    let n1 = read entrada1

    entrada2 <- getLine
    let n2 = read entrada2

    --putStrLn $ show $ intervalo n1 n2
    --putStrLn $ show $ desfazerListaDeLista (gerarListaDeLista (\x -> [soma (divisores (sequencia x) x)]) (intervalo n1 n2))
    let vetFinal = compararListas (intervalo n1 n2) 
                (desfazerListaDeLista
                    (gerarListaDeLista (\x -> [soma (divisores (sequencia x) x)])
                    (intervalo n1 n2)))

    putStrLn $ show (vetFinal !! 0) -- Defeituosos
    putStrLn $ show (vetFinal !! 1) -- Perfeitos
    putStrLn $ show (vetFinal !! 2) -- Abundantes


-- Intervalo: cria uma lista com os números inteiros entre os dois número recebidos
intervalo :: Integer -> Integer -> [Integer]
intervalo a b = [a .. b]

-- gerarListaDeLista: dado uma lista, gera uma lista de listas em que a lista mais interior segue a função f
gerarListaDeLista :: (Integer -> [Integer]) -> [Integer] -> [[Integer]]
gerarListaDeLista _ [] = []
gerarListaDeLista f (x:xs) = f x : gerarListaDeLista f xs

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

-- Soma: dada uma lista de inteiros, soma todos seus elementos
soma :: [Integer] -> Integer
soma [] = 0
soma (x:xs) = x + soma xs

-- DesfazerListaDeLista: concatena listas (operador ++) (usada com a função soma)
desfazerListaDeLista :: [[Integer]] -> [Integer]
desfazerListaDeLista [] = []
desfazerListaDeLista (xs:xss) = xs ++ desfazerListaDeLista xss

-- CompararListas: compara duas listas (a e b), a primeira (a) sendo uma sequência de inteiros e a segunda (b) sendo a soma de seus divisoes; gera uma lista de 3 posições [x, y, z] em que o primeiro elemento (x) é a quantidade de inteiros defeituosos, a segunda (y) perfeitos e a terceira (z) abundantes
compararListas :: [Integer] -> [Integer] -> [Integer]
compararListas a b = compararAux a b 0 0 0
    where
        compararAux [] [] x y z = [x, y, z] -- Caso as listas sejam vazias, vai retornar [0,0,0]
        compararAux (a:as) (b:bs) x y z -- a: sequencia de inteiros, b: soma dos divisores
            | a > b = compararAux as bs (x+1) y z -- defeituosos
            | a == b = compararAux as bs x (y+1) z -- perfeitos
            | a < b = compararAux as bs x y (z+1) -- abundantes