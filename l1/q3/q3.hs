isReplica :: String -> Int -> Char -> Bool
isReplica [] 0 _ = True
isReplica [] n _ = False
isReplica (x:xs) n c = x == c && isReplica xs (n - 1) c

main :: IO ()
main = do
    a <- getLine
    b <- getLine
    c <- getChar
    let result = isReplica a (read b) c
    print result