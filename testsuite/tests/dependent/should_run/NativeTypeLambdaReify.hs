{-# LANGUAGE DataKinds, ExperimentalDependentTypes, Modifiers, PolyKinds, TemplateHaskell, TypeFamilies, TypeOperators #-}
module NativeTypeLambdaReify where

import qualified Data.Kind as K
import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(Refl))
import GHC.Exts (U)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (addModFinalizer, mkNameG_tc)
import System.Directory (doesFileExist, removeFile)
import TypeLambda (L, withErrors)

$(do info <- reify ''L
     case info of
       TyConI (TySynD _ [] rhs) -> pure [TySynD (mkName "RoundTrip") [] rhs]
       _ -> fail "expected the type synonym")

checked :: RoundTrip :~: '[Maybe Int, Either String Int, IO Int]
checked = Refl

-- The local signature and the imported signature each contain a private name.
local :: Proxy (\a -> Maybe a)
local = Proxy

$(do let copy original label = do
           ty <- reifyType original
           let name = mkName label
           pure [SigD name ty, ValD (VarP name) (NormalB (VarE original)) []]
     concat <$> sequence [copy 'local "copyLocal", copy 'withErrors "copyImported"])

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

type CapturedCaseLet a b =
  let captured = '(a, a) in case b of
    'True -> Proxy captured
    'False -> Maybe (Proxy captured)

$(do loc <- location
     let occ = "$case-let-rhs:CapturedCaseLet:0"
         helper = mkNameG_tc (loc_package loc) (loc_module loc) occ
     visible <- lookupTypeName occ
     canReify <- recover (pure False) (True <$ reify helper)
     canReifyType <- recover (pure False) (True <$ reifyType helper)
     case (visible, canReify, canReifyType) of
       (Nothing, False, False) -> pure ()
       _ -> fail "the generated case-let helper escaped through Template Haskell"
     public <- reify ''CapturedCaseLet
     case public of
       TyConI (TySynD name [_, _] _) | name == ''CapturedCaseLet -> pure []
       _ -> fail "expected the public case-let synonym")
