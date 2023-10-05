{-# Language GADTs #-}
module T14098 where

data App f a where
  App :: f a -> App f (Maybe a)

data Ty a where
  TBool :: Ty Bool
  TInt  :: Ty Int

data T f a where
  C :: T Ty (Maybe Bool)

f1 :: T f a -> App f a -> ()
f1 C (App TBool) = ()

f2 :: T f a -> App f a -> ()
f2 C (App x)
  | TBool <- x
  = ()

g :: T f a -> App f a -> ()
g C (App x) = case x of
                TBool -> ()
