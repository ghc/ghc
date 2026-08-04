{-# LANGUAGE TemplateHaskell, StandaloneKindSignatures, FlexibleInstances #-}

module T27608c where

import Data.Kind (Type)
import Language.Haskell.TH (lookupTypeName, reportError)
import Language.Haskell.TH.Syntax (bindCode)

type D :: forall n. n -> Type
data D a

class C a where
  meth :: a -> ()

-- #27608: same leak, reached through an instance method body.
instance C (D b) where
  meth _ = $$( bindCode (do m <- lookupTypeName "n"
                            case m of
                              Just _  -> reportError "leaked: n is in scope"
                              Nothing -> pure ())
                        (\_ -> [|| () ||]) )
