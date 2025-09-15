module RepPolyWildcardPattern where

import GHC.Exts

foo :: forall rep (a :: TYPE rep). a -> ()
foo _ = ()
