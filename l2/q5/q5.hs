data Cmd = Cursor Int
           | Backspace Int
           | Delete Int
           | Insert String
           deriving (Read)

editTextAux :: (String, Int) -> Cmd -> (String, Int)
editTextAux (s, c) (Cursor n) = (s, c + n)
editTextAux (s, c) (Delete n) = (take c s ++ drop n (drop c s), c)
editTextAux (s, c) (Backspace n) = editTextAux (s, c - n) (Delete n)
editTextAux (s, c) (Insert s2) = (take c s ++ s2 ++ drop c s, c)

editText :: String -> [Cmd] -> String
editText s l = fst $ foldl editTextAux (s, 0) l

main :: IO ()
main = do
       a <- getLine
       b <- getLine
       let result = editText a (read b)
       print result