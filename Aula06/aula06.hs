--import Prelude
-- Todo código em haskell começa com um import Prelude oculto, que contém funções como map, filter e sum
-- Para usar funções de nome igual, colocar Main. antes da função
-- Exemplo: Main.map usaria a função map que definimos no nosso código
-- Prelude.main usaria a função map definida na biblioteca Prelude

main = do
    putStrLn $ show $ sum $ map (^2) $ filter ((==1).(`mod` 2)) a

a = [3, 4, 2, 5, 1, 2, 3, 4, 7, 8]