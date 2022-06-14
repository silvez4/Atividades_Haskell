module Cadastro where
import CPF
import IO
import System
import Numeric
import Char (toUpper)

menu :: IO()
menu = 
  do
    putStrLn " "
    putStrLn " _____________________________ "
    putStrLn "|                             |"
    putStrLn "|     CADASTRO DE PESSOAS     |"
    putStrLn "|  a - Inserir Cadastro       |"
    putStrLn "|  b - Gerar Relatório        |"
    putStrLn "|  c - Busca por nome         |"
    putStrLn "|  d - Busca por CPF          |"
    putStrLn "|  e - Média da Turma         |"
    putStrLn "|  f - Excluir Cadastro       |"
    putStrLn "|  g - Sair do Sistema        |"
    putStrLn "|_____________________________|"
    putStrLn "Digite uma das Opções:"
    le_opcao

le_opcao :: IO ()
le_opcao = do {
  opcao <- getChar;
  putStr "\n";
  f_menu (toUpper opcao);
}

f_menu :: Char -> IO()
f_menu i = 
  do
    case i of
      'A' -> insere_cadastro;
      'B' -> imprimir_alunos;
      'C' -> buscar_nome;
      'D' -> buscar_cpf;
      'E' -> media_turmas;
      'F' -> excluir_cadastro;
      otherwise -> sair i
    putStrLn "Operacao Concluida"
    if not(i=='G') then menu else putStr ""

insere_cadastro :: IO ()
insere_cadastro = 
  do
    nm <- getLine -- evitar pegar espaco branco
    putStrLn "Nome: "
    nm <- getLine
    putStrLn "CPF: "
    cpf <- getLine
    putStrLn "Nota: "
    nt <- getLine
    let cadastro = (nm ++ "#" ++ cpf ++ "#" ++ nt)
    pt_arq <- abreArquivo "dados.txt" AppendMode
    if(verifCPFCadastro cpf == "valido") then hPutStrLn pt_arq cadastro else putStrLn "CPF Inválido\nAluno Não Cadastrado\n"
    fechaArquivo pt_arq
--if(verifCPFCadastro cpf == "valido") then putStr "Válido\n" else menu

imprimir_alunos :: IO ()
imprimir_alunos = 
  do
    nm <- getLine -- evitar pegar espaco branco
    putStrLn ""
    putStrLn " _____________________________ "
    pt_arq <- abreArquivo "dados.txt" ReadMode
    conteudo <- (hGetContents pt_arq)
    cadastros <- (converteConteudo (conteudo))
    imprime cadastros
    fechaArquivo pt_arq
    putStrLn " _____________________________ "
  
converteConteudo :: String -> IO [[String]]
converteConteudo conteudo = return (map (explodir '#') (explodir '\n' conteudo))

explodir :: Eq a => a-> [a] -> [[a]]
explodir a [] = []
explodir a (x:xs)
  |(takeWhile (/= a) (x:xs)) == [] = explodir a xs
  | x == a = (takeWhile (/= a) xs) : explodir a (dropWhile (/= a ) xs)
  | otherwise = (takeWhile (/= a) (x:xs)) : explodir a (dropWhile (/= a) (x:xs))

sair :: Char -> IO()
sair g
  | g == 'G' = putStrLn "Saindo do Sistema..."
  | otherwise = putStrLn "Operação Inválida..."

imprime_cadastros :: IO()
imprime_cadastros = do
  let lista = "dados.txt"
  putStrLn""
  putStrLn"_____________________________"
  pt_arq <- abreArquivo lista ReadMode
  conteudo <- (hGetContents pt_arq)
  cadastros <- (converteConteudo (conteudo))
  imprime cadastros
  putStrLn"_____________________________"

buscar_algo :: ([[String]] -> a -> IO b) -> a -> IO ()
buscar_algo funcao filtro = 
  do
    putStrLn " "
    putStrLn"_____________________________"
    pt_arq <- abreArquivo "dados.txt" ReadMode
    conteudo <- (hGetContents pt_arq)
    cadastros <- (converteConteudo (conteudo))
    funcao cadastros filtro
    fechaArquivo pt_arq
    putStrLn"_____________________________"

buscar_nome :: IO ()
buscar_nome = 
  do
    nome <- getLine
    putStrLn ""
    putStrLn "Digite o nome desejado: "
    nome <- getLine
    buscar_algo buscar_por_nome nome

buscar_por_nome :: [[String]] -> String -> IO ()
buscar_por_nome [] nm = putStrLn ""
buscar_por_nome (x:xs) nm
  |(nome x == nm) = 
      do
        putStrLn ( foldl1 (\a b->a++" "++b) x)
        buscar_por_nome xs nm
  |otherwise = buscar_por_nome xs nm

buscar_cpf :: IO()
buscar_cpf = do
  cpf <- getLine
  putStrLn ""
  putStrLn "Digite o CPF desejado: "
  cpf <- getLine
  buscar_algo buscar_por_cpf cpf

buscar_por_cpf :: [[String]] -> String -> IO ()
buscar_por_cpf [] cp  = putStrLn ""
buscar_por_cpf (x:xs) cp
  |(cpf x == cp) = do
      putStrLn (foldl1 (\a b->a++ " " ++b) x)
      buscar_por_cpf xs cp
  |otherwise = buscar_por_cpf xs cp

imprime :: [[[Char]]] -> IO()
imprime [] = putStrLn ""
imprime (x:xs) = do
  putStrLn (foldl1 (\a b->a++ " "++b) x)
  imprime xs

abreArquivo :: String -> IOMode -> IO Handle
abreArquivo arquivo modo = 
  catch (openFile arquivo modo)
    (\_ -> do {
      putStrLn ("Impossivel abrir " ++ arquivo);
      putStrLn ("Será aberto com um nome default: dados.txt e limpo");
      pt_arq <-abreArquivo "dados.txt" WriteMode;
      fechaArquivo pt_arq;
      abreArquivo "dados.txt" ReadMode
    })

excluir_cadastro :: IO ()
excluir_cadastro = do
  nome <- getLine
  putStrLn "O cadastro será apagado pelo nome."
  putStrLn "Digite o nome desejado:"
  nome <- getLine
  pt_arq <- abreArquivo "dados.txt" ReadMode
  conteudo <- (hGetContents pt_arq)
  cadastros <- (converteConteudo (conteudo))
  let novo_conteudo = apaga_p_nome cadastros nome
  aux_pt_arq <- abreArquivo "auxiliar.txt" WriteMode
  hPutStr aux_pt_arq novo_conteudo
  fechaArquivo aux_pt_arq
  fechaArquivo pt_arq
  copiar "auxiliar.txt" "dados.txt"

apaga_p_nome :: [[String]] -> String -> String
apaga_p_nome [] nm = "\n"
apaga_p_nome (x:xs) nm
  | nm == (nome x) = (apaga_p_nome xs nm)
  | otherwise = (foldl1 (\a b->a++"#"++b) x) ++ "\n" ++ (apaga_p_nome xs nm)

media_turmas :: IO()
media_turmas =
  do
    nm <- getLine
    pt_arq <- abreArquivo "dados.txt" ReadMode
    conteudo <- (hGetContents pt_arq)
    cadastros <- (converteConteudo (conteudo))
    putStrLn (show (media_turma cadastros))
    fechaArquivo pt_arq

media_turma :: [[String]] -> Float
media_turma [] = 0
--media_turma x = (soma_turma x) / (fromIntegral(length x :: Int))
media_turma x = (soma_turma x) / fromIntegral (length x) :: Float

soma_turma :: [[String]] -> Float
soma_turma [] = 0
soma_turma (x:xs) = (read (nota x) :: Float) + (soma_turma xs)

nome,cpf,nota :: [String] -> String

nome (a:b:c:[]) = a
cpf (a:b:c:[]) = b
nota (a:b:c:[]) = c

copiar origem destino = do
  pt_arq <- abreArquivo origem ReadMode
  conteudo <- (hGetContents pt_arq)
  aux_pt_arq <- abreArquivo destino WriteMode
  hPutStr aux_pt_arq conteudo
  fechaArquivo aux_pt_arq
  fechaArquivo pt_arq

fechaArquivo :: Handle -> IO ()
fechaArquivo handle_arq = hClose handle_arq

