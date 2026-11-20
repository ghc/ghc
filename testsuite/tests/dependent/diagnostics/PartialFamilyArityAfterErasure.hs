{-# LANGUAGE DataKinds, LiberalTypeSynonyms, Modifiers, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE NoExperimentalDependentTypes #-}
module PartialFamilyArityAfterErasure where

import Data.Kind (Type)
import GHC.Matchability (U)
import Language.Haskell.TH qualified as TH

type family Select (b :: Bool) (a :: Type) :: Type where
    Select 'True a = Maybe a
    Select 'False a = [a]

data Accept (f :: Type %U -> Type)
type Hide a = Int

-- Only the second field keeps its partial family during validity checking.
checked :: ( Hide (Accept (Select 'True))
           , Accept ($(pure (TH.ConT ''Select)) 'False) )
checked = undefined
