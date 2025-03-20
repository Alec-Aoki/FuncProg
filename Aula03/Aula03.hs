-- Generalizando códigos

main = do
    putStrLn $ show a
    --putStrLn $ show $ soma a

    --putStrLn $ show $ somaPos a
    --putStrLn $ show $ somaNeg a
    --putStrLn $ show $ somaPar a

    --putStrLn $ show $ somaSe "Pos" a
    --putStrLn $ show $ somaSe "Neg" a
    --putStrLn $ show $ somaSe "Par" a

    --putStrLn $ show $ somaTeste ehPositivo a
    --putStrLn $ show $ somaTeste ehNegativo a
    --putStrLn $ show $ somaTeste ehPar a

    --putStrLn $ show $ somaTeste (\x -> x > 0) a -- Positivo
    --putStrLn $ show $ somaTeste (\x -> x < 0) a -- Negativo
    --putStrLn $ show $ somaTeste (\x -> mod x 2 == 0) a -- Par
    --putStrLn $ show $ somaTeste (\x -> mod x 2 == 1) a -- Ímpar

    --putStrLn $ show $ operaFunc (\x -> True) soma 0 a
    --putStrLn $ show $ operaFunc (\x -> x > 0) soma 0 a
    --putStrLn $ show $ operaFunc (\x -> x > 0) multiplica 1 a

    -- Forma final kkk
    --putStrLn $ show $ operaFunc (>0) (+) 0 a
    --putStrLn $ show $ operaFunc (>0) (*) 1 a

    putStrLn $ show $ soma $ filtra (\x -> x > 0) a -- Soma positivos

--soma x y = x + y
multiplica x y = x * y

a = [3, 5, 1, -9, 42, -50, 90, 10]

soma [] = 0
soma (x:xs) = x + soma xs

-- Usando ctrl c ctrl v (ruim)
somaPos [] = 0
somaPos (x:xs)
    |   x > 0 = x + somaPos xs
    |   otherwise = somaPos xs

somaNeg [] = 0
somaNeg (x:xs)
    |   x < 0 = x + somaNeg xs
    |   otherwise = somaNeg xs

somaPar [] = 0
somaPar (x:xs)
    |   mod x 2 == 0 = x + somaPar xs
    |   otherwise = somaPar xs

-- Solução menos pior, mas ainda ruim
somaSe cond [] = 0
somaSe cond (x:xs)
    |   cond == "Pos" && x > 0 = x + somaSe cond xs
    |   cond == "Neg" && x < 0 = x + somaSe cond xs
    |   cond == "Par" && mod x 2 == 0 = x + somaSe cond xs
    |   otherwise = somaSe cond xs

-- Solução melhor: FUNÇÃO DE ALTA ORDEM (função que recebe outra função como argumento)
somaTeste teste [] = 0
somaTeste teste (x:xs)
    |   teste x = x + somaTeste teste xs -- Soma os x que passarem no teste (+ genérico)
    |   otherwise = somaTeste teste xs

-- somaTeste não precisa ser alterado, estamos criando novos testes para ele
ehPositivo x = x > 0 -- Função que retorna True se x > 0
ehPar x = mod x 2 == 0
ehNegativo x = x < 0

-- FUNÇÃO COMO PRIMEIRA CLASSE, solução ainda melhor
ehPos = \x -> x > 0 -- Mesma coisa que ehPositivo, usando LAMBDA (\)

-- Função que múltiplica os valores
prodTeste teste [] = 1
prodTeste teste (x:xs)
    |   teste x = x * prodTeste teste xs -- Soma os x que passarem no teste (+ genérico)
    |   otherwise = prodTeste teste xs

-- FUNÇÃO PIKA

operaFunc teste op neutro [] = neutro
operaFunc teste op neutro (x: xs)
    |   teste x = op x $ operaFunc teste op neutro xs
    |   otherwise = operaFunc teste op neutro xs

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