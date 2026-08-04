{-# LANGUAGE TemplateHaskell, StandaloneKindSignatures #-}

module T27608a where

import Data.Kind (Type)
import Language.Haskell.TH (lookupTypeName, reportError)
import Language.Haskell.TH.Syntax (bindCode)

type D :: forall n. n -> Type
data D a

-- #27608: 'n' (kind-generalised) must not leak into tcl_rdr, so the typed
-- splice (which runs in the typechecker) must not resolve it.
leak :: D a -> ()
leak _ = $$( bindCode (do m <- lookupTypeName "n"
                          case m of
                            Just _  -> reportError "leaked: n is in scope"
                            Nothing -> pure ())
                      (\_ -> [|| () ||]) )
