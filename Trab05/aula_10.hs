import System.IO 
import Debug.Trace
import Data.Ord
import Data.List
import Data.Maybe

data Data = Data { 
    ano :: Integer, 
    mes :: Integer, 
    dia :: Integer 
  }
  deriving (Show, Read)

type Idade = Integer

type Venda = Float

data Vendedor = Vendedor {
    nome :: [Char],
    cpf :: [Char],
    uf :: [Char],
    aniversario :: Data,
    dependentes :: [Idade],
    vendas :: [Venda]
  }
  deriving (Show, Read)

main = do 
    --Todos vendedores
    putStrLn $ 
     show $ 
     vendedores
    -- Numero de vendedores
    putStrLn $ 
     show $ 
     length $
     vendedores
    -- Numero de vendedores de MG
    putStrLn $
     show $
     length $
     filter ((=="MG").uf) $
     vendedores
    -- Nome dos vendedores de SP
    putStrLn $
     show $
     map nome $
     filter ((=="SP").uf) $
     vendedores
    -- Somas das vendas do PR
    putStrLn $
     show $
     sum $ --Faz a soma das somas
     map sum $ --Soma a lista de cada um
     map vendas $ --Pega as listas das vendas de cada um
     filter ((=="PR").uf) $
     vendedores
    -- Primeiro nome dos vendedores em ordem alfabética
    putStrLn $
     show $
     sort $ --Ordena
     map head $ --Pega so o primeiro nome na lista quebrada anterior
     map words $ --Dado uma lista, quebra no espaço " ".
     map nome $
     vendedores
    -- Melhorar o item anterior, evitando repetir "map" e usando composição
    putStrLn $
     show $
     sort $
     map (head . words . nome) $ --Composição 
     vendedores
    -- Os vendedores ordenados por nome
    putStrLn $
     show $
     sortBy (comparing nome) $ --Ordenar pela comparação do nome
     vendedores
    --Nome dos 3 vendedores com a menor quantidade de dependentes menores de 18 anos
    putStrLn $
     show $
     map nome $
     take 3 $
     sortBy (comparing (length . filter (<18) . dependentes)) $
     vendedores
    --Nome dos 3 vendedores com a maior quantidade de dependentes menores de 18 anos
    putStrLn $
     show $
     map nome $
     take 3 $
     -- reverse $ --Inverter a ordem na ordenação
     -- Ou usar o flip
     sortBy (flip (comparing (length . filter (<18) . dependentes))) $
     vendedores
    --Nome dos 3 vendedores, de SP, com a maior quantidade de dependentes menores de 18 anos
    putStrLn $
     show $
     map nome $
     take 3 $
     -- reverse $ --Inverter a ordem na ordenação
     -- Ou usar o flip
     sortBy (flip (comparing (length . filter (<18) . dependentes))) $
     filter ((=="SP").uf) $
     vendedores
    putStrLn $
     show $
     map (sum . vendas) $
     filter ((>0) . length . filter (>10) . dependentes) $
     vendedores
     
     
     
vendedores :: [Vendedor]
vendedores = [
  Vendedor {nome = "Isabel Nubia de Ferreira", cpf = "859.193.420-37", uf = "SP", aniversario = Data {ano = 1970, mes = 12, dia = 8}, dependentes = [14,15], vendas = [9474,7999,1465,962,1865,4602,4556,7296,3706,7974,462,6445]},
  Vendedor {nome = "Heitor Bezerra de Guerra", cpf = "466.216.725-43", uf = "PR", aniversario = Data {ano = 1973, mes = 6, dia = 22}, dependentes = [13,14], vendas = [1825,8600,5597,181,4382,3131,2502,4928,8673,4624,5641,5279,5996,6599,4734,9139,4072]},
  Vendedor {nome = "Valter Wesley Duarte Jr.", cpf = "399.183.343-61", uf = "SP", aniversario = Data {ano = 1974, mes = 8, dia = 17}, dependentes = [0,2,6,10], vendas = [2266,8770,6796,1607,6828,4225,7980]},
  Vendedor {nome = "Celeste Itamara Brito Campos", cpf = "013.043.018-60", uf = "MG", aniversario = Data {ano = 1974, mes = 10, dia = 22}, dependentes = [3,6], vendas = [9372,2590,1870,137,8313,641]},
  Vendedor {nome = "Arlete Rosimeire Galhardo Padrao", cpf = "683.649.737-76", uf = "PR", aniversario = Data {ano = 1974, mes = 11, dia = 3}, dependentes = [3,9,13,14], vendas = [378]},
  Vendedor {nome = "Renata Davila de Faria", cpf = "238.427.177-07", uf = "SP", aniversario = Data {ano = 1976, mes = 5, dia = 31}, dependentes = [13,13,14], vendas = [2944,3003,1502,7565,5482,1852,4394,1102,9822,7820,6494,2880]},
  Vendedor {nome = "Isis Susan Barros de Domingues", cpf = "282.910.788-37", uf = "SP", aniversario = Data {ano = 1978, mes = 4, dia = 26}, dependentes = [4], vendas = [1725,1659,4031,4968,738,9668,1250,8510,5432]},
  Vendedor {nome = "Beatriz Rosa Brito de Faria", cpf = "267.064.573-31", uf = "MS", aniversario = Data {ano = 1980, mes = 3, dia = 20}, dependentes = [3,4,9], vendas = [3821,1123,6256,4310]},
  Vendedor {nome = "Natal Estrada Neto", cpf = "884.107.481-23", uf = "GO", aniversario = Data {ano = 1980, mes = 6, dia = 23}, dependentes = [], vendas = [5996,7942,9455,8406,7655,9312,245,5671,320,9323,5136,1634,5507,780,9004,8653]},
  Vendedor {nome = "Josue Aguiar de Cardoso", cpf = "150.860.037-61", uf = "PR", aniversario = Data {ano = 1981, mes = 12, dia = 4}, dependentes = [0,1,7,11,13,14], vendas = [4654,6979,1251,3534,105,4858,4696,4723,5784,6096]},
  Vendedor {nome = "Jardel Colaco de Brito", cpf = "354.928.905-58", uf = "GO", aniversario = Data {ano = 1982, mes = 10, dia = 12}, dependentes = [10,11,11,13,14,16], vendas = [3180,8939,2355,4483,8985,9796,4702,20,6517,1088,7663,7141,5663]},
  Vendedor {nome = "Cassia Cordeiro de Martines", cpf = "751.357.845-18", uf = "MG", aniversario = Data {ano = 1983, mes = 4, dia = 14}, dependentes = [1,9,12,12], vendas = [960,5065,2044,206,2035,1292,2897,920,1863,1530,5671,7178,1556,841,9324,5382,1915,2246]},
  Vendedor {nome = "Natal Nelson Casanova Queiros de Bezerra", cpf = "779.922.800-23", uf = "MG", aniversario = Data {ano = 1983, mes = 12, dia = 18}, dependentes = [3,5,5,7,10,13], vendas = [3794,3575,7422]},
  Vendedor {nome = "Jeronimo Jacomo de Corona", cpf = "347.737.108-93", uf = "SP", aniversario = Data {ano = 1986, mes = 11, dia = 28}, dependentes = [7,9,9,10,10], vendas = [8996,9570,2091,5925,8329,897,3222,3022,3884,8879,844,1461,5352,7099,4152,4013,5084]},
  Vendedor {nome = "Isabel Aparecida Batista Queiros", cpf = "836.413.729-91", uf = "PR", aniversario = Data {ano = 1987, mes = 4, dia = 22}, dependentes = [5,5,6,10], vendas = [508,2636,7090,7363,8428,5743,3168,102,245,3467,5481,9285,3259,1922,8742]},
  Vendedor {nome = "Joaquin Adriano Correia Pereira", cpf = "456.349.695-44", uf = "MG", aniversario = Data {ano = 1988, mes = 11, dia = 14}, dependentes = [1,8,12,13], vendas = [8973,6031,2174,2552,8035,9651]},
  Vendedor {nome = "Daniela Larissa Gil de Lozano", cpf = "452.837.102-14", uf = "PR", aniversario = Data {ano = 1990, mes = 8, dia = 15}, dependentes = [1,1,8,13], vendas = [1624,4774,7921,7265,1302,4600,7757,1953,1008,2052,9119,4161,7980,1645,8356,6694,3233,2569]},
  Vendedor {nome = "Luzinete Sabrina de Camacho", cpf = "553.759.486-00", uf = "PR", aniversario = Data {ano = 1992, mes = 4, dia = 13}, dependentes = [4,6,11,17], vendas = []},
  Vendedor {nome = "Bete Nair Fernandes de Mares", cpf = "154.849.945-37", uf = "SP", aniversario = Data {ano = 1993, mes = 10, dia = 3}, dependentes = [], vendas = [1093,3933,5538,5074,949,5740,5120]},
  Vendedor {nome = "Cibele Jimenes Neves", cpf = "609.045.295-47", uf = "SP", aniversario = Data {ano = 1995, mes = 5, dia = 16}, dependentes = [3,5,8,16], vendas = [1929,5609,579,6577,5105,9506,8658,2287]}
  ]