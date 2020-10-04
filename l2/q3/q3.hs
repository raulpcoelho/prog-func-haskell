data Ops = SUM | MUL | SUB
           deriving (Read)

data IntTree = Nilt Int |
               Node Ops IntTree IntTree
               deriving (Read)

evalTree :: IntTree -> Int
evalTree (Nilt x) = x
evalTree (Node SUM trL trR) = evalTree trL + evalTree trR
evalTree (Node MUL trL trR) = evalTree trL * evalTree trR
evalTree (Node SUB trL trR) = evalTree trL - evalTree trR

main :: IO () 
main = do
    s <- getLine
    let result = evalTree (read s)
    print result
