{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module T21010 ( CBind(..) ) where
import T21010A ( WrapMono, Constrained(Dom), withMonoCoercible )

class CBind m where
  (>>-) :: (Dom m a, Dom m b) => m a -> (a -> m b) -> m b

instance CBind (WrapMono ()) where
  (>>-) = withMonoCoercible undefined
