decEnigma :: String -> [(Char, Char)] -> String
decEnigma str alphabet = [b | x <- str, (a, b) <- alphabet, x == a]

main :: IO()
main = do
    a <- getLine
    b <- getLine
    let result = decEnigma a (read b)
    print result