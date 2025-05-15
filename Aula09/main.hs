-- Arquivos
import System.IO

data Curso = BCC | BSI | BCD
    deriving (Show, Read)

{-- 
data Aluno = Aluno ([Char], Integer, Curso, [Float])
    deriving (Show, Read)
--}

data Aluno = Aluno {
    nome :: [Char],
    nusp :: Integer,
    curso :: Curso,
    notas :: [Float]
    }
    deriving (Show, Read)

a1 = Aluno {
    nome = "Alec",
    nusp = 15436800,
    curso = BCC,
    notas = [10, 9, 8]
    }

a2 = a1 {
    curso = BSI
}

{--
a1 = Aluno ("Alec", 15436800, BCC, [10, 9, 8])

a2 = a1
a2[2] = BSI -- Não pode pois uma vez criado um objeto, ele não pode ser alterado
--}

main = do
    arq <- openFile "poggers.txt" ReadMode -- FOPEN
    contents <- hGetContents arq -- Lê todo o arquivo
    --hClose arq -- Daria erro por causa do lazy (só tentaria ler o arquivo quando precisar imprimir,  aí já estaria fechado)
    let ls = lines contents -- lines: dada uma string, quebra a string onde tiver \n
    let l = length ls
    putStrLn $ show l
    hClose arq

    putStrLn $ show a1
    putStrLn $ show
        $ sum
        $ notas a1