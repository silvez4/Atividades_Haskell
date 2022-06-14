calcCirc :: Float -> Float
calcCirc r = pi * (r^2)

ehPar :: Int -> String
ehPar num
	|even num  = "Par!"
	|otherwise = "Impar!"

sinal :: Int -> Int
sinal num = signum num

ehMenor :: Int -> Int -> Int -> Int
ehMenor a b c
	| a <= b && a <= c = a
	| b <= a && b <= c = b
	|otherwise = c

opAnd :: Bool -> Bool -> Bool -> Bool
opAnd a b c = a && b && c 

opOr :: Bool -> Bool -> Bool -> Bool
opOr a b c = a || b || c

opXor :: Bool -> Bool -> Bool
opXor a b = not (not (not a && b) && not (a && not b))

(#) :: String -> String -> String
(#) txt1 txt2
	| txt1 /= txt2 = txt1++ " " ++txt2
	| otherwise = txt1

calc :: Char -> Float -> Float -> Float
calc op a b
	| op == '+' = a + b
	| op == '-' = a - b
	| op == '*' = a * b
	| op == '/' && b /= 0 = a / b
	|otherwise = error "Erro"

passagem :: Float -> Int -> Float
passagem valor idade
	| idade >= 60 = valor * 0.6
	| idade < 2 = valor * 0.1
	| idade <= 10 = valor * 0.5
	| otherwise = valor