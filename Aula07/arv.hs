main = do
    putStrLn $ show "HW"
    putStrLn $ show $ addElem 20 $ addElem 5 $ addElem 8 $ addElem 20 Nula

-- Árvoe genérica do tipo a
data Arvore a = Nula | No (Arvore a) a (Arvore a)
    deriving (Eq, Show, Read)

criaNo :: a -> Arvore a
criaNo x = No Nula x Nula

addElem :: (Ord a) => a -> Arvore a -> Arvore a
addElem x Nula = criaNo x
addElem x (No e n d)
    | x == n = (No e n d)
    | x < n = No (addElem x e) n d
    | x > n = No e n (addElem x d)