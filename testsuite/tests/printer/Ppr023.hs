class AwsType a where
    toText :: a -> b


    {-# MINIMAL toText #-}

class Minimal a where
  toText :: a -> b
  {-# MINIMAL decimal, hexadecimal, realFloat, scientific #-}

class Minimal a where
  toText :: a -> b
  {-# MINIMAL shape, (maskedIndex | maskedLinearIndex) #-}

class Minimal a where
  toText :: a -> b
  {-# MINIMAL (toSample | toSamples) #-}

class ManyOps a where
    aOp :: a -> a -> Bool
    bOp :: a -> a -> Bool
    cOp :: a -> a -> Bool
    dOp :: a -> a -> Bool
    eOp :: a -> a -> Bool
    fOp :: a -> a -> Bool
    {-# MINIMAL  ( aOp)
               | ( bOp   , cOp)
               | ((dOp  |  eOp) , fOp)
      #-}

class Foo a where
    bar :: a -> a -> Bool
    foo :: a -> a -> Bool
    baq :: a -> a -> Bool
    baz :: a -> a -> Bool
    quux :: a -> a -> Bool
    {-# MINIMAL bar, (foo, baq | foo, quux) #-}
