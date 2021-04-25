{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -ddump-splices #-}
module T19737 where

import Language.Haskell.TH

type family T a

$(pure [ TySynInstD (TySynEqn Nothing (ConT ''T `AppT` ConT ''Char) (ConT ''Char))
       , TySynInstD (TySynEqn Nothing (ParensT (ConT ''T) `AppT` ConT ''Int) (ConT ''Int))
       , TySynInstD (TySynEqn Nothing (ParensT (ConT ''T `AppT` ConT ''Bool)) (ConT ''Bool))
       , TySynInstD (TySynEqn Nothing (ParensT (ParensT (ConT ''T `AppT` ParensT (ConT ''Double)))) (ConT ''Double))
       ])
