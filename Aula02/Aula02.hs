main = do
    --putStrLn $ show $ sinal_if(-10)
    --putStrLn $ show $ sinal(-10)
    --putStrLn $ show $ sinal(10)
    --putStrLn $ show $ absoluto(-5)
    --putStrLn $ show $ absoluto(5)

-- Função sinal
sinal_if x = if x < 0
            then -1
            else if x == 0
                then 0
                else 1

sinal x -- Com "guarda"
    | x < 0 = -1
    | x == 0 = 0
    | otherwise = 1

-- Função absoluto
absoluto x
    | x >= 0 = x
    | otherwise = -x

-- Guardas são interpretadas uma a uma na ordem em que elas foram colocadas
-- Otherwise é equivalente a True, portanto ele precisa ir no final das condições (é equivalente ao else)
-- Caso seja usado um parâmetro não coberto por um caso, o programa é ABORTADO, apesar dele compilar

-- Função que soma somente os números positivos
somaPos [] = 0
somaPos (x:xs) -- Soma de uma lista com cabeça e cauda
    | x > 0 = x + somaPos xs -- Se a cabeça for positivo, soma a cabeça com a cauda (recursivamente)
    | otherwise = somaPos xs -- Senão, soma a cauda (sem considerar a cabeça, também recursivamente)