{-# LANGUAGE UndecidableInstances, TypeFamilies #-}

module T21302 where

data BoxAssocDouble = BoxAssocDouble (BoxAssoc Int)
  deriving (Eq)

type family Assoc a

data BoxAssoc a = BoxAssoc (Assoc a)

deriving instance c Eq a => Eq (BoxAssoc a)
