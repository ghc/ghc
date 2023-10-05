{-# LANGUAGE DataKinds, TypeOperators, PolyKinds, FlexibleInstances, FlexibleContexts #-}

module T7095 where

data Wrapped t = Wrapped 

class Len l where
    len :: l -> Int

instance Len (Wrapped '[]) where
    len = const 0

instance (Len (Wrapped xs)) => Len (Wrapped (x ': xs)) where
    len x = 1 + (len $ wrappedTail x)   

wrappedTail :: Wrapped (x ': xs) -> Wrapped xs
wrappedTail = const Wrapped  

-- | test1 == zero just as excepted. 
test1 = len (undefined  :: Wrapped '[])

-- | Since I have typeclasses defined for Wrapped (* ': [*]) and for (Wrapped '[])
-- I except to get 1 here, but this does not typecheck with following  message:
-- No instance for (Len (Wrapped [*] ([] *)))  
test2 = len (undefined  :: Wrapped '[Int]) 
