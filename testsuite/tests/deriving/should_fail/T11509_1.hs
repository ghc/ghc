{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE StaticPointers             #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE UndecidableSuperClasses    #-}
module T11509 where

import Data.Kind
import Data.Typeable
import GHC.StaticPtr

{-------------------------------------------------------------------------------
  Standard Cloud-Haskell-like infrastructure

  See <https://gitlab.haskell.org/ghc/ghc/wikis/typeable-t> for a discussion of 'SC'.
-------------------------------------------------------------------------------}

class Serializable a -- empty class, just for demonstration purposes

instance Serializable a => Serializable [a]

data Static :: * -> * where
  StaticPtr :: StaticPtr a -> Static a
  StaticApp :: Static (a -> b) -> Static a -> Static b

staticApp :: StaticPtr (a -> b) -> Static a -> Static b
staticApp = StaticApp .  StaticPtr

data Dict :: Constraint -> * where
  Dict :: c => Dict c

class c => SC c where
  dict :: Static (Dict c)

instance (Typeable a, SC (Serializable a)) => SC (Serializable [a]) where
  dict = aux `staticApp` dict
    where
      aux :: StaticPtr (Dict (Serializable a) -> Dict (Serializable [a]))
      aux = static (\Dict -> Dict)

{-------------------------------------------------------------------------------
  Demonstrate the bug
-------------------------------------------------------------------------------}

newtype MyList a = MyList [a]

deriving instance (Typeable a, SC (Serializable a)) => SC (Serializable (MyList a))
