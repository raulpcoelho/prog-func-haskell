import Prelude hiding (Maybe (..))

data Maybe a = Just a |
               Nothing
               deriving(Show)

safeSecond :: [a] -> Maybe a
safeSecond [] = Nothing
safeSecond [x] = Nothing
safeSecond (x:x2:xs) = Just x2

main :: IO ()
main = do
       a <- getLine
       let result = safeSecond (read a::[Int])
       print result