main = do
    putStrLn "HW"
    putStrLn $ show $ horaDeAcordar Dom
    putStrLn $ show $ Dom -- não funcionaria sem a instance pois Dom é um construtor, não é um tipo fundamental
    putStrLn $ show $ elem Ter [Seg, Ter, Qua, Qui] -- "
    putStrLn $ show $ elem Jun [Jan, Fev, Mar]
    putStrLn $ show $ elem Jun [Abr .. Nov]

-- Novo tipo de dado
data DiaDaSemana = Dom | Seg | Ter | Qua | Qui | Sex | Sab
-- show é uma classe, então transformamos DiaDaSemana em uma instância dessa classe e definimos seus valores MANUALMENTE
instance Show DiaDaSemana where
    show Dom = "Domingo"
    show Seg = "Segunda"
    show Ter = "Terca"
    show Qua = "Quarta"
    show Qui = "Quinta"
    show Sex = "Sexta"
    show Sab = "Sabado"
instance Eq DiaDaSemana where
    Dom == Dom = True
    Dom == _ = False
    Sab == Sab = True
    Sab == _ = False
    Seg == Seg = True
    Seg == _ = False
    Ter == Ter = True
    Ter == _ = False
    Qua == Qua = True
    Qua == _ = False
    Qui == Qui = True
    Qui == _ = False
    Sex == Sex = True
    Sex == _ = False

data Mes = Jan | Fev | Mar | Abr | Mai | Jun | Jul | Ago | Set | Out | Nov | Dez
    deriving (Eq, Show, Read, Ord, Enum, Bounded) -- AUTOMATICAMENTE implemnta as relações fundamentais de Eq (igual a si mesmo e dif. de todo o resto), Show (nome do dado em string)

data Arvore = Nula | No Arvore Integer Arvore
    deriving (Eq, Show, Read)

criaNo :: Integer -> Arvore
criaNo x = No Nula x Nula

addElem :: Integer -> Arvore -> Arvore
addElem x Nula = criaNo x
addElem x (No e n d)
    | x == n = (No e n d)
    | x < n = No (addElem x e) n d
    | x > n = No e n (addElem x d)

horaDeAcordar :: DiaDaSemana -> (Integer, Integer) -- Hora e minuto
horaDeAcordar Dom = (9,30)
horaDeAcordar Sab = (8, 0)
horaDeAcordar _ = (7,15)