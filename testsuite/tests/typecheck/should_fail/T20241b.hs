{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module ShouldFail where

import Data.Kind
  ( Type, Constraint )
import Data.Proxy
  ( Proxy(..) )
import GHC.TypeLits
  ( TypeError, ErrorMessage(..) )

-- Check that custom type errors are still detected when they are
-- used at different kinds and applied to many arguments.

foo :: ( ( TypeError (Text "Boom") :: (Type -> Type) -> Type -> Constraint ) IO ) a
    => Proxy a -> ()
foo Proxy = ()

bar :: ( ( c :: Constraint -> Type -> Constraint )
           ( ( ( TypeError (Text "Boom") :: (Type -> Type) -> Type -> Constraint )
              IO
             )
             a
           )
       ) a
    => Proxy a -> ()
bar Proxy = ()
