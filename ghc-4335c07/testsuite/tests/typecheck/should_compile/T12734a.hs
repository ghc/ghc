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
import Control.Applicative
import Control.Monad.Fix
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Class
import Control.Monad.IO.Class


data A
data B
data Net
data Type

data Layer4 t l
data TermStore

data Stack lrs (t :: * -> *) where
    SLayer :: t l -> Stack ls t -> Stack (l ': ls) t
    SNull  :: Stack '[] t

instance ( Con m (t l)
         , Con m (Stack ls t)) => Con m (Stack (l ': ls) t)
instance Monad m               => Con m (Stack '[]       t)
instance ( expr ~ Expr t lrs
         , Con m (TStk t lrs)) => Con m (Layer4 expr Type)


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


newtype KT (cls :: *) (t :: k) (m :: * -> *) (a :: *)
   = KT (IdentityT m a)

test_ghc_err :: KT A '[Type] IO (Expr Net '[Type])

test_ghc_err = test_gr @(KT A '[Type] IO) @_ @'[Type] @(Expr Net '[Type])

{-  Works!
test_ghc_err = test_gr @(KT A '[Type] IO)
                       @Net
                       @'[Type]
                       @(Expr Net '[Type])
-}

{-  Some notes.  See comment:10 on Trac #12734

[W] Con m (TStk t lrs)
[W] Inferable A lrs m
[W] bind ~ Expr t lrs
[W] m bind ~ KT A '[Type] IO (Expr Net '[Type])

==> m := KT A '[Type] IO
    bind := Expr Net '[Type]
    t := Net
    lrs := '[Type]

[W] Con m (TStk t lrs)
  = Con m (Stack lrs (Layer4 bind))
--> inline lrs
[W] Con m (Stack '[Type] (Layer4 bind))
--> instance
[W] Con m (Stack '[] bind)
    --> Monad m
+
[W] Con m (Layer4 bind Type)
-->
[W] bind ~ Expr t0 lrs0
[W] Con m (TStk t0 lrs0)
-}
