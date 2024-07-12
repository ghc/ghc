module Settings.Builders.GenApply (
    genapplyBuilderArgs
    ) where

import Builder
import Settings.Builders.Common

genapplyBuilderArgs :: Args
genapplyBuilderArgs = mconcat
  [ builder (GenApply Nothing) ? ( arg =<< getInput )
  , mconcat
    [ builder (GenApply (Just sz)) ? mconcat
       [ arg =<< getInput
       , arg ("-V" ++ show sz) ]
    | sz <- [16, 32, 64]
    ]
   ]
