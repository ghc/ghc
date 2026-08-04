{-# LANGUAGE TemplateHaskell, StandaloneKindSignatures, ScopedTypeVariables #-}

module T27608d where

import Data.Kind (Type)
import Language.Haskell.TH (lookupTypeName, reportError)
import Language.Haskell.TH.Syntax (bindCode)

type D :: forall n. n -> Type
data D a

-- #27608: the pattern-signature codepath already scopes only renamer-bound
-- names, so 'n' does not leak.
patsig = \(_ :: D b) -> $$( bindCode (do m <- lookupTypeName "n"
                                         case m of
                                           Just _  -> reportError "leaked: n is in scope"
                                           Nothing -> pure ())
                                     (\_ -> [|| () ||]) )
