{-# OPTIONS_GHC -fplugin=Hooks.MetaPlugin -fplugin-opt=Hooks.MetaPlugin:trace-type #-}
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, TemplateHaskell #-}
module HooksRetainedSplice (choose) where

import Language.Haskell.TH

choose :: foreach (flag :: Bool) ->
  $(do Just flag <- lookupValueName "flag"
       pure (CaseT (IndexVarT flag)
         [ TypeCaseAlt (PromotedT 'True) (ConT ''Int)
         , TypeCaseAlt (PromotedT 'False) (ConT ''Char) ]))
choose True = 17
choose False = 'q'
