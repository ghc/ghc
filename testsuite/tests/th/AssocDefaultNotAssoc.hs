{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module AssocDefaultNotAssoc where

import Language.Haskell.TH

type family F a

$(do
  clsDecl <- classD (return []) (mkName "Cls") [plainTV (mkName "a")] []
                  [ tySynInstD (tySynEqn Nothing (appT (conT ''F) (varT (mkName "a"))) (conT ''Int)) ]
  return [clsDecl])
