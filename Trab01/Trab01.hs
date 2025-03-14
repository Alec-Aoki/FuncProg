main = do
    -- Recebendo valores de entrada para cada lado, e transformando eles em números
    entradaLadoA <- getLine
    let ladoA = read entradaLadoA

    entradaLadoB <- getLine
    let ladoB = read entradaLadoB

    entradaLadoC <- getLine
    let ladoC = read entradaLadoC

    putStrLn $ resposta ladoA ladoB ladoC $ checarTriangulo ladoA ladoB ladoC $ algumNegativo ladoA ladoB ladoC
        where
            resposta a b c False = "-"
            resposta a b c True = show $ calcularArea a b c

-- Confere se todos valores inseridos são positivos
algumNegativo a b c
    | a < 0 || b < 0 || c < 0 = True
    | otherwise = False

-- Confere se os lados dados podem formar um triângulo, caso não haja nenhum lado negativo
checarTriangulo a b c True = False
checarTriangulo a b c False
    | a + b >= c && a + c >= b && c + b >= a = True
    | otherwise = False

-- Calcula a área de um triângulo de acordo com a fórmula de Heron
calcularArea a b c = sqrt(p * (p - a) * (p - b) * (p - c))
    where p = (a + b + c)/2