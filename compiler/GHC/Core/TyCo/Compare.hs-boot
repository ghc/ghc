module GHC.Core.TyCo.Compare where

import GHC.Prelude
import GHC.Stack ( HasCallStack )
import {-# SOURCE #-} GHC.Core.TyCo.Rep ( Type )

-- | Compare complete types at nominal equality.
eqType :: HasCallStack => Type -> Type -> Bool
-- | Compare lists of complete types at nominal equality.
eqTypes :: [Type] -> [Type] -> Bool
