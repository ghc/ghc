{-# LANGUAGE DataKinds, ExperimentalDependentTypes, Modifiers, PolyKinds, TemplateHaskell, TypeOperators #-}
module NativeTypeLambdaReify where

import qualified Data.Kind as K
import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import GHC.Exts (U)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (addModFinalizer)
import System.Directory (doesFileExist, removeFile)
import TypeLambda (L)

$(do info <- reify ''L
     case info of
       TyConI (TySynD _ [] rhs) -> pure [TySynD (mkName "RoundTrip") [] rhs]
       _ -> fail "expected the type synonym")

checked :: RoundTrip :~: '[Maybe Int, Either String Int, IO Int]
checked = Refl

-- This marker lives in the private testsuite directory. If the renamer
-- copies the written kind splice into the lifted family, its second
-- execution fails before kind checking can hide the duplication.
type CheckedOnce
  (q :: $(do let marker = "NativeTypeLambdaReify.kind-splice"
             seen <- runIO (doesFileExist marker)
             if seen then fail "the capture kind splice ran twice" else pure ()
             runIO (writeFile marker "")
             addModFinalizer (runIO (removeFile marker))
             [t| K.Type |]))
  (a :: q) =
    ((\b -> (Proxy a, b)) :: K.Type %U -> K.Type)
