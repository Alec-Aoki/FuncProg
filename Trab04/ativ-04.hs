{--
 Atividade para simular pontos em um boliche
 São 10 frames, no qual strikes dão bonus em 2 jogadas seguidas
 e spares dão bonus em apenas 1.
 --}

data Placar = Strike 
            | Spare Int 
            | Pontos Int Int
            | Bonus Int

instance Show Placar where
  show Strike = "X _ | "
  show (Spare x) = show (x) ++ " / | "
  show (Pontos x y) = show (x) ++ " " ++ show (y) ++ " |"
  show (Bonus x) = show (x) ++ " "

iniciarPlacar :: [Int] -> [Placar]
iniciarPlacar pinos = montarPlacar 0 pinos

montarPlacar :: Int -> [Int] -> [Placar]
montarPlacar _ [] = []
montarPlacar _ (x:[]) = [Bonus x] 
montarPlacar n (x1:x2:xs)
  | n > 10 = Bonus x1 : montarPlacar (n+1) (x2:xs)
  | x1 == 10 = Strike : montarPlacar (n+1) (x2:xs)
  | x1 + x2 == 10 = Spare x1 : montarPlacar (n+1) xs
  | otherwise = Pontos x1 x2 : montarPlacar (n+1) xs

calcBonus :: [Int] -> Int
calcBonus [] = 0
calcBonus (_:[]) = 0
calcBonus (_:_:[]) = 0 
calcBonus pinos@(x1:x2:x3:xs)
  | x1 == 10 = x2 + x3 + checarCauda
  | x1 + x2 == 10 = x3 + calcBonus (x3:xs)
  | otherwise = checarCauda
  where
    checarCauda = calcBonus (tail pinos)

somaPlacar :: [Int] -> Int
somaPlacar [] = 0
somaPlacar (_:[]) = 0
somaPlacar (_:_:[]) = 0
somaPlacar (x:xs) = x + somaPlacar xs

main :: IO ()
main = do
  entrada <- getLine
  let pinosDerrubados = map read (words entrada) :: [Int]
  -- putStrLn $ show $ pinosDerrubados
  -- putStrLn $ show $ montarPlacar pinosDerrubados
  let pontuacao = (somaPlacar pinosDerrubados) + (calcBonus pinosDerrubados)
  putStrLn $ show $ pontuacao
  putStrLn $ show $ unwords $ map show (iniciarPlacar pinosDerrubados)
