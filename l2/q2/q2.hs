data Tree t = Nilt |
              Node t (Tree t) (Tree t)
              deriving (Read, Show)


insertTree :: Ord t => Tree t -> t -> Tree t
insertTree Nilt x = Node x Nilt Nilt
insertTree (Node n left right) x
    | x < n     = Node n (insertTree left x) right
    | otherwise = Node n left (insertTree right x)

insertList :: Ord t => Tree t -> [t] -> Tree t
insertList tr l = foldl insertTree tr l

main :: IO ()
main = do
       a <- getLine
       b <- getLine
       let result = insertList (read a::Tree Int) (read b)
       print result
