{-# LANGUAGE TemplateHaskell, PolyKinds, RoleAnnotations #-}

module Roles3 where

import Language.Haskell.TH

$( do { decls <- [d| data Foo a (b :: k) c@R (d :: k)@N |]
      ; reportWarning (pprint decls)
      ; return decls })
