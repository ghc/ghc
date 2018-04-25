{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RebindableSyntax #-}

import Prelude

class IfThenElse a b where
  ifThenElse :: a -> b -> b -> b

instance IfThenElse Bool b where
  ifThenElse c x y = if c then x else y

data Foo = Foo | Bar | Baz deriving (Eq, Ord)

main :: IO ()
main = print $ Foo < Bar
