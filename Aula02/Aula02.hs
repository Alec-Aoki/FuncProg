main = do
    putStrLn $ show $ sinal_if(-10)
    putStrLn $ show $ sinal(-10)
    putStrLn $ show $ sinal(10)
    putStrLn $ show $ absoluto(-5)
    putStrLn $ show $ absoluto(5)

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

