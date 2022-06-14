--putStrLn(fazCompra tabelaMercadorias [0,1234,1111,4719])
type Nome = String
type Preco = Int
type Codigo = Int
type Carrinho = [Codigo]
type Conta = [(Nome,Preco)]
type Mercadorias = [(Codigo, Nome, Preco)]

tamanhoLinha :: Int
tamanhoLinha = 30

formatarCentavos :: Int -> String
formatarCentavos a
  |a<100 = "0." ++ show a
  |otherwise = show (div a 100) ++ "." ++ show (mod a 100)

formatarLinha :: (Nome,Preco) -> String
formatarLinha (a,b) = a ++ replicate (tamanhoLinha - length(a ++ formatarCentavos b)) '.' ++ formatarCentavos b ++ "\n"

formatarLinhas :: [(Nome,Preco)] -> String
--formatarLinhas lin = show (map (\(a,b) -> formatarLinha (a,b)) lin)
--formatarLinhas lin = foldr(++) "" (map formatarLinha lin)
formatarLinhas lin = foldr(++) "" . map formatarLinha $ lin
--formatarLinhas lin = foldr(++) "" [formatarLinha (a,b) | (a,b) <- lin]
--formatarLinhas [] = []
--formatarLinhas ((a,b):xs) = formatarLinha (a,b) ++ formatarLinhas xs

formatarTotal :: Preco -> String
formatarTotal p = "\n" ++ "Total" ++ replicate (tamanhoLinha - length("Total" ++ formatarCentavos p)) '.' ++ formatarCentavos p

calculaTotal :: Conta -> Preco
calculaTotal cont = sum (map (\(x,y) -> y) cont)
--calculaTotal cont = sum [y | (x,y) <- cont]

formatarConta :: Conta -> String
formatarConta cont = "Haskell Stores\n"++ "\n" ++ (formatarLinhas cont) ++ (formatarTotal $ calculaTotal cont)

procurarCodigo :: Mercadorias -> Codigo -> (Nome,Preco)
procurarCodigo [] cod = ("Unknown Item", 0)
procurarCodigo ((x,y,z):xs) cod
  |cod == x = (y,z)
  |cod /= x = procurarCodigo xs cod 

criarConta :: Mercadorias -> Carrinho -> Conta
criarConta banc car = (map (\(x) -> procurarCodigo banc x) car) 

fazCompra :: Mercadorias -> Carrinho -> IO()
fazCompra banc car = putStrLn (formatarConta $ criarConta banc car)

tabelaMercadorias :: Mercadorias
tabelaMercadorias = [(4719,"Fish Fingers",121)
                    ,(5643,"Nappies",1010)
                    ,(3814,"Orange Jelly",56)
                    ,(1111,"Hula Hoops",21)
                    ,(1112,"Hula Hoops (Giant)",133)
                    ,(1234,"Dry Sherry, 1lt",540)
                    ]