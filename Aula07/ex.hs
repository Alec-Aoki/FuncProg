main = do
    putStrLn $ show "HW"
    putStrLn $ show $ encontra (>10) [15, 20, 25, 30]
    putStrLn $ show $ encontra (<10) [15, 20, 25, 30]
    putStrLn $ show $ multTest1 (>7) (<4) [4, 5, 2, 10, 19, 3, 21]
    putStrLn $ show $ multTest2 (>7) (<4) [4, 5, 2, 10, 19, 3, 21]

data Talvez a = Nada | Algum a
    deriving(Eq, Ord, Show, Read)

encontra :: (a -> Bool) -> [a] -> Talvez a
encontra _ [] =  Nada
encontra teste (x:xs)
    | teste x == True = Algum x
    | otherwise = encontra teste xs

multTest1 :: (Num a) => (a -> Bool) -> (a -> Bool) -> [a] -> a
multTest1 t1 t2 l =
    case encontra t1 l of
        Nada -> 0
        Algum x1 -> case encontra t2 l of
            Nada -> 0
            Algum x2 -> x1 * x2

multTest2 :: (Num a) => (a -> Bool) -> (a -> Bool) -> [a] -> Talvez a
multTest2 t1 t2 l =
    case encontra t1 l of
        Nada -> Nada
        Algum x1 -> case encontra t2 l of
            Nada -> Nada
            Algum x2 -> Algum (x1 * x2)