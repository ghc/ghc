{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# OPTIONS_GHC -Wno-unused-foralls #-}
module T15437A where

import Language.Haskell.TH.Syntax (Q, TExp)
import Language.Haskell.TH.Lib.Internal
import Data.Kind

{-
get :: forall a. Int
get = 1

foo :: forall (alpha :: Type) . LiftT alpha => Q (TExp Int)
foo = [|| get @alpha ||]

foo2 :: forall (alpha :: Type) . LiftT alpha => Q (TExp Int)
foo2 = [|| get @(Int, alpha) ||]

foo3 :: forall (alpha :: Type) . LiftT alpha => Q (TExp Int)
foo3 = [|| get @(Eq alpha) ||]

--foo4 :: forall alpha . LiftT alpha => Q (TExp (alpha -> alpha))
--foo4 = [|| id :: alpha -> alpha ||]

foo5 :: forall (alpha :: Type) (beta :: Type).
           (LiftT alpha, LiftT beta)
        => Q (TExp Int)
foo5 = [|| get @(alpha, beta) ||]
-}

foo6 :: (LiftT a, Show a) => Q (TExp (a -> String))
foo6 = [|| show ||]


