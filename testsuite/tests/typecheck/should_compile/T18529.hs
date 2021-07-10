{-# LANGUAGE Haskell2010 #-}

{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}

module Bug where

class C a b where
  op :: a -> b -> ()

-- GHC should not infer an ambiguous type signature
-- foo :: (C a b0, Num b0) => a -> ()
foo x = op x 3
