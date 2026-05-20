{-# LANGUAGE TemplateHaskell #-}
module TH_InvalidTopDecl where

import Language.Haskell.TH.Syntax

$(do decls <- [d| data Foo |]   -- #10853
     addTopDecls decls
     return [])
