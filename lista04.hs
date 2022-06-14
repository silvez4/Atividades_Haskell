--1E
maior :: [Int] -> Int
maior [] = 0
maior (x:xs)
	|x > maior xs = x
	|otherwise = maior xs

--2
produtorio :: [Int] -> Int
produtorio [] = 1
--produtorio (x:xs) = (*) x (produtorio xs)
produtorio (x:xs) = x * produtorio xs

--3
--somaElementos [[Int]] -> [Int]
--somaElementos [] = 0
--somaElementos

--4
multiplos :: Int ->(Int,Int) -> [Int]
multiplos x:(a,b)
	|x /a == 0 = x multiplos(x,(xs))
	|otherwise multiplos(x,(xs))