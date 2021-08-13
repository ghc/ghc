{-# OPTIONS_GHC -dcore-lint #-}
{-# OPTIONS_GHC -fplugin RewritePlugin #-}

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE DataKinds, ScopedTypeVariables, TypeFamilies #-}

module TcPlugin_Rewrite where

import Data.Kind
  ( Type )

import Definitions
  ( Add, Nat(..) )


foo :: forall (proxy :: Nat -> Type) (n :: Nat)
    .  ( Add Zero n ~ n )
    => proxy n -> ()
foo _ = ()

bar :: forall (proxy :: Nat -> Type) (n :: Nat)
    .  proxy n -> ()
bar n = foo n
