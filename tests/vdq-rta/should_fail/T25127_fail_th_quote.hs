{-# LANGUAGE TemplateHaskell #-}

module T25127_fail_th_quote where

import System.IO
import Language.Haskell.TH

-- Test the error message until the TH AST is extended with visible forall in GADTs.
$([d| data VisProxy a where
        VP :: forall a -> VisProxy a
    |])

-- When such code becomes valid and this test case is removed, do not forget to
-- update GadtConSigs_th_dump1 and GadtConSigs_th_pprint1.