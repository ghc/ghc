{-# LANGUAGE ExplicitNamespaces #-}

module T25901_imp_unused_3 where

-- The goal of this test case is to test the interaction between
-- the -Wunused-imports warning and namespace-specified wildcard
-- imports `type ..` and `data ..`.
--
-- It was initially written with the following import decls:
--
--   import qualified T25901_helper_3 as TM ( D1, type .., D2 )
--   import qualified T25901_helper_3 as DM ( data MkD1, data .., data MkD2 )
--
-- However, they had to be split into multiple decls to work around #26527,
-- an issue that prevents -Wunused-imports from firing (unrelated to wildcards).
--
-- TODO: Combine the import decls once #26527 is fixed.

import qualified T25901_helper_3 as TM ( D1 )
import qualified T25901_helper_3 as TM ( type .. )
import qualified T25901_helper_3 as TM ( D2 )

import qualified T25901_helper_3 as DM ( data MkD1 )
import qualified T25901_helper_3 as DM ( data .. )
import qualified T25901_helper_3 as DM ( data MkD2 )

d1 :: TM.D1
d1 = DM.MkD1

d2 :: TM.D2
d2 = DM.MkD2