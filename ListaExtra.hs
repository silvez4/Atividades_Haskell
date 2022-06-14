-- Gabriel Silva de Araujo
-- 1 Construa uma função que realize a inversão de uma lista
inverte :: [Int] -> [Int]
inverte [] = []
inverte (x:xs) = inverte(xs) ++ [x]

-- 2 Construa uma função que realize o cálculo do valor, sendo informados a base e o expoente.
potencia :: Int -> Int -> Int
potencia x y
  | y == 0 = 1
  | y == 1 = x
  | otherwise = x * potencia x (y-1)

-- 3 Defina uma função recursiva que dada uma lista de inteiros, retorna uma nova lista contendo os elementos de valor superior a um número n qualquer.
retornaSup :: Int -> [Int] -> [Int]
retornaSup a [] = []
retornaSup a (x:xs)
  | x > a = x : retornaSup a (xs)
  | otherwise = retornaSup a (xs)

-- 4 Implemente uma função que receba um parâmetro inteiro n e retorne todos os divisores de n.
divisores :: Int -> [Int]
divisores a = [x | x <- [1..a], mod a x == 0]

-- 5 Retorne, a partir da função divisores implementada na questão anterior, todos os números primos de um intervalo. Para realizar esta tarefa, faça duas versões da função: uma recursiva e outra baseada em lista por compreensão. primos 
--Baseda em Lista por Compreensão
primos :: (Int,Int) -> [Int]
primos (a,b) = [x | x <- [a..b], length(divisores(x)) == 2]

-- 5 b Recursiva
primosRec :: (Int,Int) -> [Int]
primosRec (a,b)
  | a > b = []
  | length(divisores a) == 2 = a : primosRec((a+1),b)
  | otherwise = primosRec((a+1),b)