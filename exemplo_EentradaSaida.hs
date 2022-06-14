
escreverLinha :: String -> IO ()
escreverLinha str = putStrLn str


escrever4Vezes :: String -> IO ()
escrever4Vezes str = do
	putStrLn str
	putStrLn str
	putStrLn str
	putStrLn str

leitura :: IO ()
leitura = do
	linha <- getLine
	putStrLn ("Linha digitada: " ++ linha)

getInt :: IO Integer
getInt = do
	n <- getLine
	return (read n :: Integer)

soma2Valores :: IO Float
soma2Valores = do
	n1 <- getLine
	n2 <- getLine
	return ((read n1 :: Float) + (read n2 :: Float))

somaInteiros :: Integer -> IO Integer
somaInteiros soma = do
	n <- getInt 
	if n == 0 then return soma else somaInteiros (soma + n) 

somatorio :: IO ()
somatorio = do
	putStrLn "Digite valores inteiros. Um em cada linha."
	putStrLn "Eles serao somados ate voce digitar zero"
	total <- somaInteiros 0
	putStr "O somatorio eh igual a "
	print total


acumulador :: [Int] -> Int
acumulador [] = 0
acumulador (x:xs) = x + acumulador xs














