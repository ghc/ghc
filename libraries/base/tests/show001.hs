-- !!! Testing Show on Maybes and Eithers
module Main(main) where

x :: Maybe ()
x = Nothing

main :: IO ()
main = do
  print x
  print (Just ())
  print ((Just (Just ())) :: Maybe (Maybe ()))
  print (Just x)
  print ((Left 'a')  :: Either Char Int)
  print ((Right 'b') :: Either Int Char)
  print ((Right x)   :: Either Int (Maybe ()))
  print ((Right (Just  'c')) :: Either Int (Maybe Char))
  print ((Right (Right 'd')) :: Either Int (Either Char Char))
  print ((Right (Left  'e')) :: Either Int (Either Char Int))
  print ((Left 'f') :: Either Char Int)
  print ((Left x) :: Either (Maybe ()) Char)
  print ((Left (Just  'g')) :: Either (Maybe Char) ())
  print ((Left (Right 'h')) :: Either (Either Int Char) Char)
  print ((Left (Right 'i')) :: Either (Either Int Char) ())

