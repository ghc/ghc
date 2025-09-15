{-# OPTIONS_GHC -fdefer-type-errors #-}
    -- This line is crucial to the bug

{-# Language GADTs #-}
{-# Language InstanceSigs #-}
{-# Language KindSignatures #-}
{-# Language TypeFamilies #-}
{-# Language DataKinds #-}
{-# Language FlexibleInstances #-}

module T14607 where

import Data.Kind

data LamCons :: Type -> Type -> () -> Type where
  C :: LamCons a a '()

class Mk a where
  mk :: LamCons a a '()

instance Mk a where
  mk :: LamCons a '()
  mk = mk

-- At some point, this program was accepted. That's fine. But right now,
-- it's rejected with a kind error, and we can't generally defer kind
-- errors, so I'm saying that behavior is OK.

-- Later (May 18) the kind error ended up being in an term-level
-- implication constraint, which /does/ have an evidence binding
-- So now the kind error can be deferred.
-- Consequence of a fast-path for tcImplicitTKBndrsX I think.

-- Later (Nov 18) we are back to a kind error, which is fine
