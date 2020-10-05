data Command = Forward Int | Backward Int | TurnLeft | TurnRight 
               deriving (Eq, Show, Read)


destinationAux :: ((Int, Int), (Int, Int)) -> Command -> ((Int, Int), (Int, Int))
destinationAux (st, (0, 1)) TurnLeft = (st, (-1, 0))
destinationAux (st, (1, 0)) TurnLeft = (st, (0, 1))
destinationAux (st, (0, -1)) TurnLeft = (st, (1, 0))
destinationAux (st, (-1, 0)) TurnLeft = (st, (0, -1))
destinationAux (st, (0, 1)) TurnRight = (st, (1, 0))
destinationAux (st, (1, 0)) TurnRight = (st, (0, -1))
destinationAux (st, (0, -1)) TurnRight = (st, (-1, 0))
destinationAux (st, (-1, 0)) TurnRight = (st, (0, 1))
destinationAux ((x, y), (dx, dy)) (Forward n) = ((x + dx*n, y + dy*n), (dx, dy))
destinationAux ((x, y), (dx, dy)) (Backward n) = ((x - dx*n, y - dy*n), (dx, dy))

destination :: (Int,Int) -> [Command] -> (Int,Int)
destination st l = fst $ foldl destinationAux (st, (0,1)) l

main :: IO ()
main = do
       a <- getLine
       b <- getLine
       let result = destination (read a) (read b)
       print result