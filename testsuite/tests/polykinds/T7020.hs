{-# LANGUAGE KindSignatures, RankNTypes, PolyKinds, GADTs,
              FlexibleContexts, DataKinds, TypeFamilies #-}

module T7020 where

import GHC.Exts

data family Sing (a :: k)

class SingKind (Any :: k) => SingI (s :: k) where
   sing :: Sing s

data SingInstance (a :: k) where
   SingInstance :: SingI a => SingInstance a

class (b ~ Any) => SingKind (b :: k) where
   singInstance :: forall (a :: k). Sing a -> SingInstance a
