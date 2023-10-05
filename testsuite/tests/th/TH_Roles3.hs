{-# LANGUAGE TemplateHaskell, PolyKinds, RoleAnnotations #-}

module Roles3 where

import Language.Haskell.TH
import System.IO

$( do { decls <- [d| data Foo a (b :: k) c (d :: k)
                     type role Foo _ _ representational nominal |]
      ; runIO $ putStrLn (pprint decls) >> hFlush stdout
      ; return decls })
