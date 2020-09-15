btoi :: String -> Int
btoi [] = 0
btoi ('0':xs) = btoi xs
btoi ('1':xs) = 2^(length xs) + btoi xs

main :: IO()
main = do
    s <- getLine
    let result = btoi s
    print result