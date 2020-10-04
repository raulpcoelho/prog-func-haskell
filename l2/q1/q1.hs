data Command = Forward Int | Backward Int | TurnLeft | TurnRight 
               deriving (Eq, Show, Read)

data Direction = North | South | West | East
                 deriving (Read, Show)

faces_aux :: Direction -> Command -> Direction
faces_aux dir (Forward x) = dir
faces_aux dir (Backward x) = dir
faces_aux North TurnLeft = West
faces_aux North TurnRight = East
faces_aux South TurnLeft = East
faces_aux South TurnRight = West
faces_aux East TurnLeft = North
faces_aux East TurnRight = South
faces_aux West TurnLeft = South
faces_aux West TurnRight = North


faces :: Direction -> [Command] -> Direction
faces dir l = foldl faces_aux dir l

main :: IO ()
main = do
       a <- getLine
       b <- getLine
       let result = faces (read a) (read b)
       print result