main = do
    -- Recebendo strings do teclado
    la <- getLine -- <- indica que é impuro, a main agora é impura
    let a = read la -- Transformando string em número, automaticamente (essa função é pura btw)
    lb <- getLine
    let b = read lb -- read é o oposto do show
    lc <- getLine
    let c = read lc

    -- A conversão automática só funciona pq usamos a como parâmetro da função bhaskara, e ela usa esse parâmetro como número
    -- Como estamos utilizando funções impuras, as ordens do código impuro fazem diferença

    putStrLn $ show $ bhaskara a b c -- Impura

    -- let res = bhaskara a b c seria PURA pois bhaskara é pura

    -- RESUMINDO: no caso de "variáveis", usar <- para funções impuras e let = para funções puras 

    --putStrLn $ show $ sinal_if(-10)
    --putStrLn $ show $ sinal(-10)
    --putStrLn $ show $ sinal(10)
    --putStrLn $ show $ absoluto(-5)
    --putStrLn $ show $ absoluto(5)

    --putStrLn $ show $ bhaskara 1 4 10

    --putStrLn $ show $ somaPos2 [-100, 1, 2, 3]

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

-- Bhaskara c/ guarda e where
bhaskara a b c
    | delta < 0 = []
    | delta == 0 = [x]
    | otherwise = [x', x'']
        where
            delta = b^2 - 4*a*c
            x = -b/(2*a)
            x' = (-b + sqrt delta)/(2*a)
            x'' = (-b - sqrt delta)/(2*a)

-- somaPos com where
somaPos2 [] = 0
somaPos2 (x:xs)
    | x > 0 = x + somaCauda
    | otherwise = somaCauda
        where
            somaCauda = somaPos xs