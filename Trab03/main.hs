-- Ler a e b
-- Criar a seq. entre a e b
-- Percorrer a seq., verificando por números primos
    -- Criar lista de divisores do número sendo verificado
    -- Caso essa lista seja vazia, é primo
    -- Senão, não é
-- Calcular as diferenças entre elementos consecutivos desse vetor
-- Imprimir o maior valor desse vetor

main = do
    -- Recebendo a e b
    entrada1 <- getLine
    let a = read entrada1

    entrada2 <- getLine
    let b = read entrada2

    let vetFinal = quicksort $ mapa (\(x, y) -> y - x) $ pares $ filtra primo [a .. b]
    putStrLn $ show $ head $ vetFinal ++ [0] -- Coloca 0 no final para o caso em que há somente um/nenhum primo no intervalo

-- Filtra: cria uma lista somente com os itens da lista que obedecem o teste
filtra :: (a -> Bool) -> [a] -> [a]
filtra _ [] = []
filtra teste (x:xs)
    |   teste x = x:filtra teste xs
    |   otherwise = filtra teste xs

-- Mapa: aplica uma operação a todos os itens de uma lista
mapa :: (a -> b) -> [a] -> [b]
mapa _ [] = []
mapa f (x:xs) = f x:mapa f xs

-- Primo: retorna True se o número for primo, False senão
primo :: Integer -> Bool
primo x
    | x < 2 = False -- 1 não é primo
    | otherwise = not (checarDivisores [2 .. (x-1)]) -- Verificar se x tem divisores no intervalo de 2 até x-1
    where
        checarDivisores lista = or (mapa (\y -> mod x y == 0) lista) -- Dado uma lista e um número x, verifica se cada elemento dessa lista é ou não um divisor de x. Gera um vetor de booleanos contendo os resultados dessas operações, utilizando a função mapa. Realiza a operação lógica or com todos os elementos da lista, o que retornará False se não houver nenhum divisor na lista original ou True se houver ao menos um divisor. Logo, caso seja retornado False, sabemos que x é primo. Retornaremos, então True (aplicar função not na saída desse resultado).

-- Pares: dado uma lista, gera uma lista cujos elementos são pares de elementos consecutivos da lista original
pares :: [Integer] -> [(Integer, Integer)]
pares [] = []
pares [_] = []
pares (x:y:xs) = (x,y) : pares (y:xs)

-- Quicksort invertido (ordem decrescente)
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (pivot:xs) = maiores ++ iguais ++ menores
    where
        menores = quicksort $ filtra (< pivot) xs
        iguais = pivot:filtra (== pivot) xs
        maiores = quicksort $ filtra (> pivot) xs