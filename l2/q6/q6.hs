data Tree t = Nilt |
              Node t (Tree t) (Tree t)
              deriving (Read)
              
quicksort :: Ord t => [t] -> [t]
quicksort [] = []
quicksort (x:xs) = quicksort (filter (<x) xs) ++ [x] ++ quicksort (filter (>=x) xs)
              
inOrder :: Tree t -> [t]
inOrder Nilt = []
inOrder (Node n left right) = inOrder left ++ [n] ++ inOrder right

isBST :: Ord t => Tree t -> Bool
isBST t = let x = inOrder t
          in x == quicksort x
  
main :: IO ()
main = do
       s <- getLine
       let result = isBST (read s::Tree Int)
       print result