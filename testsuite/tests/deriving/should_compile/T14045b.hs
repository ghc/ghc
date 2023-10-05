{-# LANGUAGE TypeFamilies, KindSignatures, GADTs, GeneralizedNewtypeDeriving #-}

module T14045b where

import Data.Kind ( Type )

data family T a b :: Type

-- newtype instance T Int d = MkT (IO d)

newtype instance T Int :: Type -> Type where
   MkT :: IO d -> T Int d
  deriving( Monad, Applicative, Functor )
