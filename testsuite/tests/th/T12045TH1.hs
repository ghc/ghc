{-# LANGUAGE TemplateHaskell, DataKinds, PolyKinds
             , TypeInType, TypeApplications, TypeFamilies  #-}

module T12045TH1 where
import Data.Kind
import Language.Haskell.TH hiding (Type)

$([d| type family F (a :: k) :: Type where
                     F @Type Int = Bool
                     F @(Type->Type) Maybe = Char |])


$([d| data family D (a :: k) |])

$([d| data instance D @Type a = DBool |])

$([d| data instance D @(Type -> Type) b = DChar |])
