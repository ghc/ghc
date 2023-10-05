{-# LANGUAGE Haskell2010 #-}

{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, AllowAmbiguousTypes #-}

module Bug where

class C a b where
  op :: a -> b -> ()

-- GHC should infer
-- foo :: (C a b0, Num b0) => a -> ()
-- This might actually be callable, if we have e.g. instance b ~ Bool => C Int b
foo x = op x 3
