-- 1º soma de quadrados de uma lista
somaQuadrados :: Num a => [a] -> a
somaQuadrados [] = 0
somaQuadrados (x:xs) = x*x + somaQuadrados xs

-- em somente uma linha
soma :: [Int] -> Int
soma [] = 0
soma (x:xs) = x + soma xs

somaQuad :: [Int] -> Int
somaQuad lista = soma([x^2 | x <- lista])

-- 2º receber lista de tuplas com cinco numeros inteiros em cada, e retornar a mediana
qSort :: Ord t => [t] ->[t]
qSort [] = []
qSort (x:xs)= qSort [y | y <- xs, y<=x] ++ [x] ++ qSort[y | y <- xs, y > x]

--medianas :: [(Int)] -> [Int]

-- 3º retornas apenas os primos de uma lista
divisores :: Int -> [Int]
divisores a = [x | x <- [1..a], mod a x == 0]

apenasPrimos :: [Int] -> [Int]
apenasPrimos [] = []
apenasPrimos (x:xs)
  | length(divisores(x)) == 2 = x:apenasPrimos xs
  | otherwise = apenasPrimos xs

-- 4º Pesquisar o numero na posicao desejada
pesquisa :: Int -> [Int] -> Int
pesquisa n (x:xs)
  | n < 1 || n > length(x:xs) = -1
  | n == 1 = x
  | otherwise = pesquisa (n-1) xs

--EXTRA
--hello :: IO()
--hello = putStrLn "Hello World"