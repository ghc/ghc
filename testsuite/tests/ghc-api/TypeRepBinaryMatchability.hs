{-# LANGUAGE DataKinds, ExperimentalDependentTypes, Modifiers, TypeFamilies #-}

module Main (main) where

import Data.Kind (Type)
import qualified Data.Binary as Package
import GHC.Matchability (M, U)
import qualified GHC.Utils.Binary as Binary
import Type.Reflection
import Type.Reflection.Unsafe

type family Open (a :: Type) :: Type
data WithUnmatchableKind (f :: Type %U -> Type)

-- The compiler cannot synthesize a TypeRep for an open family. Use the
-- reflected kind of WithUnmatchableKind's parameter for the opaque head.
opaque :: SomeTypeRep
opaque = case tyConKindRep (typeRepTyCon (typeRep @WithUnmatchableKind)) of
    KindRepFun kind _ -> SomeTypeRep (App' (headRep kind) (typeRep @Int))
    _ -> error "expected a matchable arrow kind for WithUnmatchableKind"
  where
    headRep :: KindRep -> TypeRep Open
    headRep kind = mkTrCon (mkTyCon "main" "Main" "Open" 0 kind) []

roundTrip :: String -> SomeTypeRep -> SomeTypeRep -> IO ()
roundTrip label expected rep = do
    out <- Binary.openBinMem 1024
    Binary.put_ out rep
    decoded <- Binary.shrinkBinBuffer out >>= Binary.get
    check "GHC.Utils.Binary" decoded
    check "Data.Binary" (Package.decode (Package.encode rep))
  where
    check codec decoded = case decoded of
        SomeTypeRep (App' f _)
          | Fun' match _ _ <- typeRepKind f
          , SomeTypeRep match == expected
          , decoded == rep -> putStrLn (codec ++ " " ++ label)
        _ -> fail (codec ++ " decoded a different " ++ label)

main :: IO ()
main = do
    roundTrip "matchable application" (SomeTypeRep (typeRep @M)) (SomeTypeRep (typeRep @(Maybe Int)))
    roundTrip "unmatchable application" (SomeTypeRep (typeRep @U)) opaque
