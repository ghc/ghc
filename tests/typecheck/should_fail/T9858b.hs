{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module T9858b where
import Data.Typeable

test = typeRep (Proxy :: Proxy (Eq Int => Int))



