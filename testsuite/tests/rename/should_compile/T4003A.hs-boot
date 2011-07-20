module T4003A where

import Data.Data

data HsExpr i

instance Typeable1 HsExpr
instance Data i => Data (HsExpr i)
