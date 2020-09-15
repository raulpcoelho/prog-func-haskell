--Funcao que separa a string recebida em uma lista de strings menores
separa :: String -> Int -> String -> [String] -> [String]
separa [] _ _ lista = lista
separa (x:xs) i substring lista
    | mod i 3 /= 0 && x == ' ' = separa xs i (';':substring) lista
    | mod i 3 /= 0 && x /= ';' = separa xs i (x:substring) lista
    | mod i 3 /= 0 && x == ';' = separa xs (i+1) (x:substring) lista
    | mod i 3 == 0 && x /= ';' = separa xs i (x:substring) lista
    | otherwise = separa xs (i+1) [] (substring:lista)

-- Funcao que inverte o resultado de separa para que a lista fique na ordem dada
separa2 :: [String] -> [String]
separa2 lista = reverse lista 

-- Funcao que recebe uma string e converte para tupla
funcao :: String -> (String, String, String, String) -> Int -> (String, String, String, String)
funcao [] resultado _ = resultado
funcao (';':xs) resultado i = funcao xs resultado (i + 1)
funcao (x:xs) (a,b,c,d) i
    | i == 1 = funcao xs (a, b, c, x:d) i
    | i == 2 = funcao xs (a, b, x:c, d) i
    | i == 3 = funcao xs (a, x:b, c, d) i
    | i == 4 = funcao xs (x:a, b, c, d) i

--Funcao que converte os parametros data e valor para int e double
aux1 :: (String, String, String, String) -> (Int, String, String, Double)
aux1 (a,b,c,d) = (read a::Int, b, c, read d::Double)

-- Funcao que converte uma lista de strings para uma lista de tuplas
stringsParaTuplas :: [String] -> [(Int, String, String, Double)]
stringsParaTuplas [] = []
stringsParaTuplas (x:xs) = (aux1 (funcao x ([],[],[],[]) 1)) : stringsParaTuplas xs

--Funcao que usa as funcoes acima para agrupar a string recebida em uma lista de tuplas
tuplasFinal :: String -> [(Int, String, String, Double)]
tuplasFinal s = stringsParaTuplas (separa2 (separa s 1 [] []))

--Funcao desejada
logMes :: String -> String -> Double
logMes mes fatura = resultado
    where resultado = sum [d | (a,b,c,d) <- (tuplasFinal fatura), mes == b]

main :: IO()
main = do
    a <- getLine
    b <- getLine
    let result = logMes a b
    print result