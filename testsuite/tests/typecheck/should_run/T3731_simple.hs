{-# LANGUAGE
  FlexibleContexts,
  FlexibleInstances,
  ScopedTypeVariables,
  TypeApplications,
  UndecidableInstances
#-}

module Bug where

class Default a
class Sat a

instance Default a => Sat a

class Sat a => Data a where
  dataTypeOf :: a -> a

defaultDefaultValue :: forall a. Data a => a
defaultDefaultValue = res
    where
    --res :: a
      res = dataTypeOf @a res -- (slightly different error without the type application)


-- Uncommenting the type signature of res, or enabling MonoLocalBinds, allows the program to compile.
