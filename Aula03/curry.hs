-- Currying

main = do
    putStrLn $ show $ soma $ filtra (\x -> x > 0) a -- Soma positivos


a = [3, 5, 1, -9, 42, -50, 90, 10]

soma [] = 0
soma (x:xs) = x + soma xs

-- Composição de funções?
-- Todas essas funções são equivalentes, e na verdade multiplica só tem 1 parâmetro
multiplica x y = x * y
multiplica' x = \y -> x * y
multiplica'' = \x -> (\y -> x * y)

-- Função final
filtra teste [] = []
filtra teste (x:xs)
    | teste x = x:r
    | otherwise = r
        where
            r = filtra teste xs

-- Função final final
filtraFinal _ [] = [] -- Teste não é importante nessa linha, então colocamos _
filtraFinal teste (x:xs)
    | teste x = x:r
    | otherwise = r
        where
            r = filtraFinal teste xs

ehPositivo x = x > 0 -- Função que retorna True se x > 0

multiplica x y = x * y

somaSe cond [] = 0
somaSe cond (x:xs)
    |   cond == "Pos" && x > 0 = x + somaSe cond xs
    |   cond == "Neg" && x < 0 = x + somaSe cond xs
    |   cond == "Par" && mod x 2 == 0 = x + somaSe cond xs
    |   otherwise = somaSe cond xs

somaTeste teste [] = 0
somaTeste teste (x:xs)
    |   teste x = x + somaTeste teste xs -- Soma os x que passarem no teste (+ genérico)
    |   otherwise = somaTeste teste xs

-- Definição de Tipos MANUAL
k :: Integer -- Um número inteiro, SEM LIMITE DE TAMANHO

m :: Int -- Inteiro limitado ao espaço da memória de 4 bytes (int em C)

a :: [Integer] -- Lista de integers

ehPositivo :: Integer -> Bool -- FUNÇÃO de integer para booleano

-- ehPositivo :: Integer -> Integer NÃO COMPILARIA

multiplica :: Integer -> (Integer -> Integer)

somaSe :: [Char] -> [Integer] -> Integer

somaTeste :: (Integer -> Bool) -> [Integer] -> Integer

filtra :: (Integer -> Bool) -> [Integer] -> [Integer] -- Recebe uma função (que transforma um inteiro em um booleano) e um vetor de inteiros, e retorna um vetor de inteiros

-- Generalizando filtra: filtra :: (a -> Bool) -> [a] -> [a], onde a é um outro tipo

