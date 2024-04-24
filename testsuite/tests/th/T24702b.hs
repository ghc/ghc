{-# LANGUAGE TemplateHaskell, MagicHash #-}
module T24702b where

import Language.Haskell.TH

data Unit = MkUnit
tup0 :: $(conT (mkName "Unit"))
tup0 = MkUnit

data Solo = MkSolo
tup1 :: $(conT (mkName "Solo"))
tup1 = MkSolo

data Tuple2 = MkTuple2
tup2 :: $(conT (mkName "Tuple2"))
tup2 = MkTuple2

data CUnit = MkCUnit
ctup0 :: $(conT (mkName "CUnit"))
ctup0 = MkCUnit

data CSolo = MkCSolo
ctup1 :: $(conT (mkName "CSolo"))
ctup1 = MkCSolo

data CTuple2 = MkCTuple2
ctup2 :: $(conT (mkName "CTuple2"))
ctup2 = MkCTuple2

data Unit# = MkUnit#
utup0 :: $(conT (mkName "Unit#"))
utup0 = MkUnit#

data Solo# = MkSolo#
utup1 :: $(conT (mkName "Solo#"))
utup1 = MkSolo#

data Tuple2# = MkTuple2#
utup2 :: $(conT (mkName "Tuple2#"))
utup2 = MkTuple2#

data Sum2# = MkSum2#
sum2 :: $(conT (mkName "Sum2#"))
sum2 = MkSum2#
