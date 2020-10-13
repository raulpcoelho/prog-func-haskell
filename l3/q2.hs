import Prelude hiding (Maybe (..))

data Maybe a = Just a |
               Nothing
               deriving(Show)

isDigit :: Char -> Bool
isDigit x
    | x >= '0' && x <= '9' = True
    | otherwise            = False

calcAux :: Int -> String -> Int -> Maybe Int
calcAux x "sum" y = Just (x + y)
calcAux x "sub" y = Just (x - y)
calcAux x "mul" y = Just (x * y)
calcAux x "div" 0 = Nothing
calcAux x "div" y = Just (div x y)
calcAux _ _ _ = Nothing

safeCalc :: String -> IO ()
safeCalc s = let num1 = read (takeWhile (isDigit) s)
                 f    = takeWhile (not.isDigit) (dropWhile (isDigit) s)
                 num2 = read (dropWhile (not.isDigit) (dropWhile (isDigit) s))
             in print (calcAux num1 f num2)

main :: IO ()
main = do
       a <- getLine
       safeCalc a