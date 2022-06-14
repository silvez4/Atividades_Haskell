-- 1º 3 METODOS PARA MULTIPLICAR TODOS DA LISTA POR 2
x2com :: [Int] -> [Int]
x2com list = [a*2 | (a) <- list]

x2rec :: [Int] -> [Int]
x2rec [] = []
x2rec (x:xs) = x*2 : x2rec xs


x2map :: [Int] -> [Int]
x2map list = (map (*2) list)

-- 2º RETORNA TAMANHO DE UMA LIST COM SUM E MAP
tamanho :: [Int] -> Int
tamanho list = sum (map (^0) list)

{- 3º FILTRO PARA SOMAR +1 CASO NUMERO SEJA MAIOR Q 1
  ***** BASE MODELO
incrementaLista :: [Int] -> [Int]
incrementaLista lista = filter maiorQueUm (map incrementa lista)
 where
    maiorQueUm n = n > 1
    incrementa n = n + 1
  ***** FIM BASE MODELO
-}
--incrementaLista lista = filter(>1) (map (+1) lista)
incrementaLista lista = (map (+1) (filter(>0) lista))

-- 4º Inter n f x = função f ocorre n vezes f(f(f.....(f x)))
dobro :: Int -> Int
dobro x = x*2

iter :: Num a => a -> (a -> a) -> a -> a
iter 0 f x = x 
iter n f x = iter (n-1) f (f x) 
{-
  | n > 0 = Iter (n-1) (f x) x
  | otherwise = x
-}