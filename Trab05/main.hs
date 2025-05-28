-- Faça UM programa, que leia uma linha com 4 inteiros separados por espaço. Sejam esses inteiros n1, n2, n3 e n4. Ele deve imprimir as seguintes informações:
--    A soma de "Active" de todos os países em que "Confirmed" é maior o igual que n1.
--    Dentre os n2 países com maiores valores de "Active", a soma das "Deaths" dos n3 países com menores valores de "Confirmed".
--    Os n4 países com os maiores valores de "Confirmed". Os nomes devem estar em ordem alfabética.
-- As colunas do arquivo são: Country, Confirmed, Deaths, Recovery, Active.

-- Alec Campos Aoki, 15436800

import System.IO
import Data.Ord
import Data.List

-- Struct Pais
data Pais = Pais {
    country :: String,
    confirmed :: Int,
    deaths :: Int,
    recovery :: Int,
    active :: Int
    }
    deriving (Show, Read)

main = do
    arquivo <- openFile "dados.csv" ReadMode -- fopen
    conteudo <- hGetContents arquivo -- Lendo todo o arquivo
    let linhas = lines conteudo -- Pega uma linha do arquivo (separação no \n)
        paises = map quebrarPais linhas -- Transformando as linhas do arquivo em países
    
    let paisesValidos = [paisesFiltro | Just paisesFiltro <- paises] -- Filtro para pegar somente [Pais] em vez de [Maybe Pais]

    -- Leitura dos inteiros
    entrada <- getLine
    let [n1, n2, n3, n4] = map read (words entrada) :: [Int]


    -- Soma de "Active" de todos os países em que "Confirmed" é maior o igual que n1
    putStrLn $
        show $
        sum $ -- Soma da lista
        map active $ -- Lista com todos os valores de active onde n1 > confirmed
        filter ((> n1).confirmed) $
        paisesValidos

    -- Dentre os n2 países com maiores valores de "Active", a soma das "Deaths" dos n3 países com menores valores de "Confirmed"
    putStrLn $
        show $
        sum $
        map deaths $
        take n3 $
        sortBy (comparing (confirmed)) $
        take n2 $
        sortBy(flip (comparing (active))) $
        paisesValidos

    -- Os n4 países com os maiores valores de "Confirmed". Os nomes devem estar em ordem alfabética.
    mapM_ putStrLn $
        sort $
        map country $
        take n4 $
        sortBy (flip (comparing(confirmed))) $
        paisesValidos

    hClose arquivo

-- Quebra UMA string em um vetor de strings nas vírgulas
quebrarVirgula :: String -> [String]
quebrarVirgula [] = []
quebrarVirgula string =
    let (campo, resto) = break (== ',') string
    in campo : case resto of
        [] -> []
        (_:restoString) -> quebrarVirgula restoString

-- Transforma uma string em uma struct do tipo país
quebrarPais :: String -> Maybe Pais
quebrarPais string = case quebrarVirgula string of -- Quebra a linha do arquivo em um vetor de strings de acordo com a função quebrarVirgula
    [nome, confirmados, mortos, resgatados, ativos] -> -- Vetor de strings mencionado acima
        Just Pais { -- Construção da struct
            country = nome,
            confirmed = read confirmados,
            deaths = read mortos,
            recovery = read resgatados,
            active = read ativos
        }
    _ -> Nothing -- Erro