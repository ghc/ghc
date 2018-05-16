module Main where

main :: IO ()
main = do
  _ <- return $ getInt Circle
  return ()

newtype MyInt = MyInt Int

data Shape = Circle | Square

getInt :: Shape -> MyInt
getInt sh =
  case sh of
    Circle ->
        let (MyInt i) = MyInt 3
        in myInt i
    Square ->
        let (MyInt i) = MyInt 2
        in myInt i
    where
      myInt = MyInt
