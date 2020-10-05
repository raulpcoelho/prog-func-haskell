data Command = Forward Int | Backward Int | TurnLeft | TurnRight 
               deriving (Eq, Show, Read)

data Direction = North | South | West | East
                 deriving (Read, Show)

facesAux :: Direction -> Command -> Direction
facesAux dir (Forward x) = dir
facesAux dir (Backward x) = dir
facesAux North TurnLeft = West
facesAux North TurnRight = East
facesAux South TurnLeft = East
facesAux South TurnRight = West
facesAux East TurnLeft = North
facesAux East TurnRight = South
facesAux West TurnLeft = South
facesAux West TurnRight = North


faces :: Direction -> [Command] -> Direction
faces dir l = foldl facesAux dir l

main :: IO ()
main = do
       a <- getLine
       b <- getLine
       let result = faces (read a) (read b)
       print result