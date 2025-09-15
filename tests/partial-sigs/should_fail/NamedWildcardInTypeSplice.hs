{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedWildCards #-}
module NamedWildcardInTypeSplice where

import Language.Haskell.TH

metaType :: TypeQ
metaType = [t| _a -> _a |]
