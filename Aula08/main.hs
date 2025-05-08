-- Função que aplica op em uma lista
-- foldr :: (a -> b -> b) -> b -> [a] -> b
--              op          base  lista  base
-- foldr op base [] = base
-- foldr op base (x:xs) = op x (foldr op base xs)

-- Soma de todos os elementos
-- sum = foldr (+) 0

-- Produto de todos os elementos
-- prod = foldr (*) 1

-- Como fazer map usando foldr? (Não precisa kk)
-- map = foldr (:.f) []

-- E filter, max e min? (Desafio)

-- Em outras linguas, foldr geralmente é chamado de reduce

main = do
    putStrLn "HW"
    putStrLn $ show a
    putStrLn $ show b
    putStrLn $ show c

    putStrLn "Monad"
    f
    f'


a = [1..10]
b = map (^2) $ filter ((==1).(`mod` 2)) a -- Igual à linha c
-- List comprehension
c = [x^2 | x <- a, x `mod` 2 == 1] -- c igual a x^2 tq. x pertence a a e x mod 2 == 1

-- Quicksort
qs [] = []
qs (x:xs) = menores ++ iguais ++ maiores
    where
        menores = qs [y | y <- xs, y < x]
        iguais = [y | y <- xs, y == x]
        maiores = qs [y | y <- xs, y > x]


-- Monad
f = do
    la <- getLine
    let a = read la
    putStrLn $ show $ a + 1

f' = getLine >>= \la -> let a = read la in putStrLn (show (a+1)) -- A função >>= (bind) basicamente repassa o resultado da função anterior a ele para a função seguinte a ele
-- Podemos falar que o f' é puro, pois só haverá efeito colateral quando f' for usada na main
g = f' 6 -- É puro

--main :: IO () -- IO é similar ao void em C
--putStrLn :: IO ()
--getLine :: IO [Char]

-- Função que lê um número e RETORNA esse número + 1
h :: IO Integer
h = do
    la <- getLine
    let a = read la
    return (a + 1) -- Usado para retornar valores quando estivermos usando monad; simplesmente (a + 1) daria erro pois não usa monad e h usa monad

-- (>>=) :: (IO a) -> (a-> IO b) -> (IO b) Mto específico
-- (>>=) :: (Monad m) => (m a) -> (a -> m b) -> (m b)

-- Usando maybe
prodNgPos 1 lista = case find (< 0) lista of
    Nothing -> Nothing
    Just a -> case find (> 0) lista of
        Nothing -> Nothing
        Just b -> Just (a * b)

-- Usando monad2
prodNegPos2 :: (Ord a, Num a) => [a] -> Maybe a
prodNegPos2 lista = do
    a <- find (< 0) lista
    b <- find (> 0) lista
    return $ (a * b)

-- Usando monad e bind explícito
prodNegPos3 lista = find (< 0) lista >>= \a -> find (> 0) lista >>= \b -> return (a * b)