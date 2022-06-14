--1
data Pergunta = Sim | Nao deriving (Eq,Show)

pergNum :: Pergunta -> Int
pergNum Sim = 1
pergNum Nao = 0

listPerg :: [Pergunta] -> [Int]
listPerg [] = []
listPerg(x:xs)
  |x == Sim = [1]++listPerg xs
  |x == Nao = 0:listPerg xs

and' :: Pergunta -> Pergunta -> Pergunta
and' a b
  |a==b = Sim
  |otherwise = Nao

or' :: Pergunta -> Pergunta -> Pergunta
or' Sim _ = Sim
or' _ Sim = Sim
or' _ _ = Nao

--2
data Temperatura = Celsius | Kelvin | Farenheit deriving (Eq,Show)

converterCelsius :: Double -> Temperatura -> Double
converterCelsius val temp  
  |temp == Celsius = val
  |temp == Kelvin = ((val*9.0/5.0)+32.0)
  |temp == Farenheit = (val+273.15)

converterKelvin :: Double -> Temperatura -> Double
converterKelvin val temp  
  |temp == Celsius = (val-273.15)
  |temp == Kelvin = val
  |temp == Farenheit = ((val-273.15)*9.0/5.0+32.0)

converterFarenheit :: Double -> Temperatura -> Double
converterFarenheit val temp  
  |temp == Celsius = ((val-32.0)*5.0/9.0)
  |temp == Kelvin = ((val-32.0)*5.0/9.0+273.15)
  |temp == Farenheit = val

soma :: Num a => [a] -> a
soma [] = 0
soma(x:xs) = x+soma xs

tamanho :: [t] -> Int
tamanho [] = 0
tamanho (x:xs)= 1+tamanho xs