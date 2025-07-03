module GHC.Core.FieldInstEnv
  ( FieldInstEnv, 
    emptyFieldEnv,
    lookupFieldEnv,
    extendFieldEnv,
    plusFieldEnv
  ) where

import GHC.Types.Unique.DFM 
import GHC.Types.FieldLabel (FieldLabel)
import GHC.Unit.Module.ModDetails (FieldInst, FieldInfo)
import GHC.Prelude.Basic

type FieldInstEnv = UniqDFM FieldLabel FieldInfo

emptyFieldEnv :: FieldInstEnv
emptyFieldEnv = emptyUDFM

lookupFieldEnv :: FieldInstEnv -> FieldLabel -> Maybe FieldInfo
lookupFieldEnv = lookupUDFM

extendFieldEnv :: FieldInstEnv -> [FieldInst] -> FieldInstEnv
extendFieldEnv env [] = env -- Should be a most common case
extendFieldEnv env flds = addListToUDFM env flds

plusFieldEnv :: FieldInstEnv -> FieldInstEnv -> FieldInstEnv
plusFieldEnv = plusUDFM