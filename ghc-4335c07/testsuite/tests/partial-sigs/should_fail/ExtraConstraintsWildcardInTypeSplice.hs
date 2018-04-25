{-# LANGUAGE TemplateHaskell #-}
module ExtraConstraintsWildcardInTypeSplice where

import Language.Haskell.TH

metaType :: TypeQ
metaType = [t| _ => _ |]
