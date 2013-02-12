module T4003A where

import Data.Data

data HsExpr i

instance Typeable HsExpr
instance Data i => Data (HsExpr i)
