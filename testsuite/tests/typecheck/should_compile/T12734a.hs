{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE TypeApplications          #-}


-- This version is shorter than T12734, and should yield a
-- type error message.  If things go wrong, you get
-- an infinite loop

module T12734a where

import Prelude
import Data.Kind
import Control.Applicative
import Control.Monad.Fix
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Class
import Control.Monad.IO.Class


data A
data B
data Net
data Ty

data Layer4 t l
data TermStore

data Stack lrs (t :: Type -> Type) where
    SLayer :: t l -> Stack ls t -> Stack (l ': ls) t
    SNull  :: Stack '[] t

instance ( Con m (t l)
         , Con m (Stack ls t)) => Con m (Stack (l ': ls) t)
instance Monad m               => Con m (Stack '[]       t)
instance ( expr ~ Expr t lrs
         , Con m (TStk t lrs)) => Con m (Layer4 expr Ty)


newtype Expr  t lrs    = Expr (TStk t lrs)
type TStk t lrs = Stack lrs (Layer4 (Expr t lrs))


class Con m t


-- HERE IS A FUNNY BEHAVIOR: the commented line raises context reduction stack overflow
test_gr :: forall m t lrs bind.
           ( Con m (TStk t lrs)
            , bind ~ Expr t lrs
--        ) => m (Expr t lrs)       -- GHC 8 worked if you said this...
          ) => m bind               -- but not this!
test_gr = undefined


newtype KT (cls :: Type) (t :: k) (m :: Type -> Type) (a :: Type)
   = KT (IdentityT m a)

test_ghc_err :: KT A '[Ty] IO (Expr Net '[Ty])

test_ghc_err = test_gr @(KT A '[Ty] IO) @_ @'[Ty] @(Expr Net '[Ty])

{-  Works!
test_ghc_err = test_gr @(KT A '[Ty] IO)
                       @Net
                       @'[Ty]
                       @(Expr Net '[Ty])
-}

{-  Some notes.  See comment:10 on #12734

[W] Con m (TStk t lrs)
[W] Inferable A lrs m
[W] bind ~ Expr t lrs
[W] m bind ~ KT A '[Ty] IO (Expr Net '[Ty])

==> m := KT A '[Ty] IO
    bind := Expr Net '[Ty]
    t := Net
    lrs := '[Ty]

[W] Con m (TStk t lrs)
  = Con m (Stack lrs (Layer4 bind))
--> inline lrs
[W] Con m (Stack '[Ty] (Layer4 bind))
--> instance
[W] Con m (Stack '[] bind)
    --> Monad m
+
[W] Con m (Layer4 bind Ty)
-->
[W] bind ~ Expr t0 lrs0
[W] Con m (TStk t0 lrs0)
-}
