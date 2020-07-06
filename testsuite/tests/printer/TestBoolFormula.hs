module TestBoolFormula where

class ManyOps a where
    aOp :: a -> a -> Bool
    aOp = undefined
    bOp :: a -> a -> Bool
    bOp = undefined
    cOp :: a -> a -> Bool
    cOp = undefined
    dOp :: a -> a -> Bool
    dOp = undefined
    eOp :: a -> a -> Bool
    eOp = undefined
    fOp :: a -> a -> Bool
    fOp = undefined
    {-# MINIMAL  ( aOp)
               | ( bOp   , cOp)
               | ((dOp  |  eOp) , fOp)
      #-}

class Foo a where
    bar :: a -> a -> Bool
    foo :: a -> a -> Bool
    baq :: a -> a -> Bool
    baq = undefined
    baz :: a -> a -> Bool
    baz = undefined
    quux :: a -> a -> Bool
    quux = undefined
    {-# MINIMAL bar, (foo, baq | foo, quux) #-}

instance Foo Int where
    bar = undefined
    baz = undefined
    quux = undefined
    foo = undefined
