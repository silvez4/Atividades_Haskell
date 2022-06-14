module CPF where
--putStrLn (verificar "111.111.111-11")
import System.IO
tamanhoLinha :: Int
tamanhoLinha = 40

verifCPFCadastro :: String -> String
verifCPFCadastro cpf
  |((verifD1 $limpar cpf) == "valido") && 
  ((verifD2 $limpar cpf) == "valido") 
  = "valido"
  |otherwise = "invalido"

verificar :: String -> IO()
verificar cpf
  |((verifD1 $limpar cpf) == "valido") && 
  ((verifD2 $limpar cpf) == "valido") 
  = putStrLn (formatarLinha cpf "valido")
  |otherwise = putStrLn (formatarLinha cpf "invalido")

veriflista :: [String] -> [String]
veriflista [] = []
veriflista (x:xs)
  |((verifD1 $limpar x) == "valido") && 
  ((verifD2 $limpar x) == "valido") 
  = (formatarLinha x "valido") : veriflista xs
  |otherwise = (formatarLinha x "invalido") : veriflista xs

limpar :: [Char] -> [Integer]
limpar cpf = [read [x]::Integer | x<-cpf, x/='.' && x/='-']


formatarLinha :: String -> String -> String
formatarLinha cpf val
  |(val == "valido") = cpf ++ replicate(tamanhoLinha - length(cpf ++ "valido")) '.' ++ "válido"
  |otherwise = cpf ++ replicate(tamanhoLinha - length(cpf ++ "invalido")) '.' ++ "inválido"

verif :: [Integer] -> Integer -> Integer
verif (x:xs) cont
  | cont >=2 = x * cont  + verif xs (cont-1)
  | otherwise = 0

verifD1 :: [Integer] -> String
verifD1 cpf
  | (length cpf) < 11 = "invalido"
  | cpf!!9 == mod ((verif cpf 10) *10) 11 = "valido"
  |otherwise = "invalido"

verifD2 :: [Integer] -> String
verifD2 cpf
  | (length cpf) < 11 = "invalido"
  | cpf!!10 == mod ((verif cpf 11) *10) 11 = "valido"
  |otherwise = "invalido"

showCPF :: String -> [String]
showCPF = words

formatar :: [String] -> String
formatar [] = "\n\nAutor: Gabriel Silva de Araujo"
formatar (x:xs) = x ++ "\n" ++ formatar xs

leitura :: IO ()
leitura = do
   let cpfs = "cpfs.txt"
   let resultado = "resultado.txt"
   content <- readFile cpfs
   -- Apenas mostrar no prompt
   --mapM_ putStrLn $ showCPF content
   --mapM_ putStrLn $ veriflista $ showCPF content
   --
   let resp = veriflista $ showCPF content
   writeFile resultado (formatar resp)
   putStrLn "Leitura e Escrita Realizada"

{- Exemplo Escrita
escrever :: IO()
escrever = do
  let resultado = "resultado.txt"
  writeFile resultado $ veriflista ["11111111151","11111111111"]
-}