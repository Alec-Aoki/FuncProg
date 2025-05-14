-- Alec Campos Aoki 15436800
-- Juan Henriques Passos 15464826

main :: IO ()
main = do
  entrada <- getLine -- Lendo a entrada como uma string
  let pinosDerrubados = map read (words entrada) :: [Int] -- Transformando cada número da entrada em um int
  let placarList = iniciarPlacar pinosDerrubados
  let placarFormatado = unwords $ map show placarList
  let pontuacao = somaPlacar 0 pinosDerrubados
  putStrLn (placarFormatado ++ " " ++ show pontuacao)

data Placar = Spare Int
            | Ponto Int
            | FimStrike
            | Fim

instance Show Placar where
  show (Spare x) = show (x) ++ " /"
  show (Ponto x)
    | x == 10 = "X" -- Strike
    | otherwise = show (x)
  show FimStrike = "_"
  show Fim = "|"

iniciarPlacar :: [Int] -> [Placar]
iniciarPlacar pinos = montarPlacar 0 pinos

montarPlacar :: Int -> [Int] -> [Placar]
montarPlacar _ [] = []
montarPlacar _ (x:[]) = [] 
montarPlacar 9 (x1:x2:x3:xs) -- Caso especial: 10a rodada
  | x1 == 10 && (x2 + x3 == 10) = Ponto x1 : Spare x2 : Fim : montarPlacar (10) [] -- Strike seguido de Spare
  | x1 == 10 = Ponto x1 : Ponto x2 : Ponto x3 : Fim : montarPlacar (10) [] -- Strike seguido de 2 pontos (podem ser strikes)
  | x1 + x2 == 10 = Spare x1 : Ponto x3 : Fim : montarPlacar (10) (xs) -- Spare
  | otherwise = Ponto x1 : Ponto x2 : Fim : montarPlacar (10) (x3:xs) -- Pontos normais
montarPlacar n (x1:x2:xs)
  | x1 == 10 = Ponto x1 : FimStrike : Fim : montarPlacar (n+1) (x2:xs) -- Strike
  | x1 + x2 == 10 = Spare x1 : Fim : montarPlacar (n+1) (xs) -- Spare
  | otherwise = Ponto x1 : Ponto x2 : Fim : montarPlacar (n+1) (xs) -- Pontos normais

somaPlacar :: Int -> [Int] -> Int
somaPlacar _ [] = 0
somaPlacar _ [x1] = 0
somaPlacar _ [x1, x2] -- 10a rodada
  | x1 == 10 || x1 + x2 == 10 = 0 -- Strike ou Spare
  | otherwise = x1 + x2 -- Pontos normais
somaPlacar n (x1:x2:x3:xs)
  | x1 == 10 = x1 + x2 + x3 + somaPlacar (n + 1) (x2:x3:xs) -- Strike
  | x1 + x2 == 10 = x1 + x2 + x3 + somaPlacar (n + 1) (x3:xs) -- Spare
  | otherwise = x1 + x2 + somaPlacar (n + 1) (x3:xs) -- Pontos normais