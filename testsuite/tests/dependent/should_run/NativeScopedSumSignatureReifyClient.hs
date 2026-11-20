{-# LANGUAGE NoExperimentalDependentTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module NativeScopedSumSignatureReifyClient where

import Control.Monad (unless)
import Data.Coerce (coerce)
import GHC.Sigma (Sigma(Pair), first)
import Language.Haskell.TH
import NativeScopedSumSignatureReifyProvider
  ( Cell(..), GStored(..), Stored(..), localRoundTrip, provided )

$(do VarI _ ty _ <- reify 'provided
     let name = mkName "importRoundTrip"
     pure [SigD name ty, ValD (VarP name) (NormalB (VarE 'localRoundTrip)) []])

$(do TyConI (NewtypeD _ _ [_] _ (RecC con [(field, _, _)]) _) <- reify ''Stored
     [NominalR] <- reifyRoles ''Stored
     VarI _ ty _ <- reify 'stored
     if con == 'Stored && field == 'stored
       then let name = mkName "unwrap"
            in pure [SigD name ty, ValD (VarP name) (NormalB (VarE 'coerce)) []]
       else fail "Unexpected stored field")

ignore :: Stored a -> Bool
ignore (Stored _) = True

main :: IO ()
main = do
  let value = Stored (Pair True (Cell (24 :: Int)))
      gadtValue = GStored (Pair False (Cell 'c'))
  unless (first (stored value) && first (unwrap value) &&
          not (first (gstored gadtValue)) && ignore (error "forced a newtype pattern" :: Stored Int)) $
    fail "The stored witness or newtype demand changed"
