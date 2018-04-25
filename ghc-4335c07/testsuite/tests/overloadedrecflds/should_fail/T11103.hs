-- When using DuplicateRecordFields with TemplateHaskell, it is not possible to
-- reify ambiguous names that are output by reifying field labels.
-- See also overloadedrecflds/should_run/overloadedrecfldsrun04.hs

{-# LANGUAGE DuplicateRecordFields, TemplateHaskell #-}

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

data R = MkR { foo :: Int, bar :: Int }
data S = MkS { foo :: Int }

$(do info <- reify ''R
     case info of
       TyConI (DataD _ _ _ _ [RecC _ [(foo_n, _, _), (bar_n, _, _)]] _)
         -> do { reify bar_n -- This is unambiguous
               ; reify foo_n -- This is ambiguous
               ; return []
               }
       _ -> error "unexpected result of reify")
