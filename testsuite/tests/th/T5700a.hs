{-# LANGUAGE TemplateHaskell #-}
module T5700a where

import Language.Haskell.TH

class C a where
  inlinable :: a -> ()

mkC :: Name -> Q [Dec]
mkC n = return
  [InstanceD [] (AppT (ConT ''C) (ConT n))
    [ FunD 'inlinable [Clause [WildP] (NormalB (ConE '())) []],
      PragmaD (InlineP 'inlinable Inline FunLike AllPhases)    
    ] 
  ]
