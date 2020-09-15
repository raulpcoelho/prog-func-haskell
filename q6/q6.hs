type Comando = String
type Valor = Int

executaAux :: [(Comando, Valor)] -> Int -> Int
executaAux [] n = n
executaAux ((c,v):xs) n
    | c == "Multiplica"       = executaAux xs (n*v)
    | c == "Soma"             = executaAux xs (n+v)
    | c == "Subtrai"          = executaAux xs (n-v)
    | c == "Divide" && v /= 0 = executaAux xs (div n v) 
    | otherwise               = -666

executa :: [(Comando, Valor)] -> Int
executa lista  = executaAux lista 0 

main :: IO()
main = do
    a <- getLine
    let result = executa (read a)
    print result
