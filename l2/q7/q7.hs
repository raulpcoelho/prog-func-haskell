data Tree t = Node t (Tree t) (Tree t) |
              Nilt
              deriving (Read)

alturaArvore :: Tree t -> Int              
alturaArvore Nilt = 0
alturaArvore (Node n left right) = 1 + max (alturaArvore left) (alturaArvore right)

main :: IO ()
main = do
       a <- getLine
       let result = alturaArvore (read a::Tree Int)
       print result
