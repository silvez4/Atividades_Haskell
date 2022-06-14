import Data.Char (ord, chr)
import System.IO

-- *** 1º ***

palindromo :: IO ()
palindromo =
  do
    putStrLn "Digite uma Palavra: "
    pl <- getLine
    if(inverter pl == pl) then putStrLn "É um Palindromo" else putStrLn "Não é um Palindromo"

inverter :: [Char] -> String
inverter [] = []
inverter (x:xs) = inverter xs ++ [x]

-- *** 2º ***

palavraPrima :: IO ()
palavraPrima =
  do
    putStrLn "Digite uma Palavra: "
    pl <- getLine
    if(prima pl) then putStrLn "A Palavra é Prima" else putStrLn "A Palavra Não é um Prima"

prima :: [Char] -> Bool
prima lista = primo $ sum (map ord lista)

divisores :: Int -> [Int]
divisores a = [x | x <- [1..a], mod a x == 0]

primo :: Int -> Bool
primo num = length(divisores num) == 2

-- *** 3º ***
convASCII :: [Char] -> [Int]
convASCII lista = map ord lista

csalt :: [Int] -> [Int]
csalt lista = map (+7) lista

criptar :: [Char] -> [Char]
criptar lista = map chr $ csalt $ convASCII lista

tsalt :: [Int] -> [Int]
tsalt lista = map (+(-7)) lista

decriptar :: [Char] -> [Char]
decriptar lista = map chr $ tsalt $ convASCII lista

criptoArq :: IO ()
criptoArq =
  do
    let txt = "texto.txt"
    let resultado = "criptografado.txt"
    content <- readFile txt
    let resp = criptar content
    writeFile resultado resp
    putStrLn "Arquivo Criptografado com sucesso!"

descriptoArq :: IO ()
descriptoArq =
  do
    let txt = "criptografado.txt"
    let resultado = "descriptografado.txt"
    content <- readFile txt
    let resp = decriptar content
    writeFile resultado resp
    putStrLn "Arquivo Descriptografado com sucesso!"