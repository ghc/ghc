{-# LANGUAGE TemplateHaskell, StandaloneKindSignatures #-}

module T27608b where

import Data.Kind (Type)
import Language.Haskell.TH (lookupTypeName, reportError)
import Language.Haskell.TH.Syntax (bindCode)

type D :: forall n. n -> Type
data D a

-- #27608: same leak, reached through a class default method body.
class C a where
  meth :: D a -> ()
  meth _ = $$( bindCode (do m <- lookupTypeName "n"
                            case m of
                              Just _  -> reportError "leaked: n is in scope"
                              Nothing -> pure ())
                        (\_ -> [|| () ||]) )
