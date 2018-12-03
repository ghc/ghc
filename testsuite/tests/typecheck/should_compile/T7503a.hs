{-# LANGUAGE ExistentialQuantification, DataKinds, PolyKinds, KindSignatures, GADTs #-}
module T7503a where
    import Data.Kind
    import GHC.Exts hiding (Any)

    data WrappedType = forall a. WrapType a

    data A :: WrappedType -> Type where
        MkA :: forall (a :: Type). AW a -> A (WrapType a)

    type AW  (a :: k) = A (WrapType a)
    type AW' (a :: k) = A (WrapType a)

    class C (a :: k) where
        aw :: AW a -- workaround: AW'

    instance C [] where
        aw = aw
