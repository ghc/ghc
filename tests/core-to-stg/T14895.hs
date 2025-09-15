module T14895 where

go :: (a -> b) -> Either String a -> Either String b
go f (Right a) = Right (f a)
go _ (Left e)  = Left e
