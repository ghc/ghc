-- Using overloaded labels as strings, slightly pointlessly

{-# LANGUAGE OverloadedLabels
           , DataKinds
           , FlexibleContexts
           , FlexibleInstances
           , MultiParamTypeClasses
           , ScopedTypeVariables
           , TypeFamilies
           , TypeSynonymInstances
  #-}

import GHC.OverloadedLabels
import Data.Proxy ( Proxy(..) )
import GHC.TypeLits ( KnownSymbol, symbolVal )

instance (KnownSymbol x, c ~ Char) => IsLabel x [c] where
  fromLabel = symbolVal (Proxy :: Proxy x)

main = do putStrLn #x
          print $ #x ++ #y
