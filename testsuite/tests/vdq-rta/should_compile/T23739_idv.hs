{-# LANGUAGE RequiredTypeArguments #-}

module T23739_idv where

idv :: forall a -> a -> a
idv t x = x :: t

idv_bsig :: forall a -> a -> a
idv_bsig t (x :: t) = x

idv_lam :: forall a -> a -> a
idv_lam = \t x -> x :: t

idv_lam_bsig :: forall a -> a -> a
idv_lam_bsig = \t (x :: t) -> x

idv_eta :: forall a -> a -> a
idv_eta = idv

idv_t2t :: forall a -> a -> a
idv_t2t t = idv t

idv_vta :: forall a -> a -> a
idv_vta t = id @t

idv_sig :: forall a -> a -> a
idv_sig t = id :: t -> t

idv_wild :: forall a -> a -> a
idv_wild _ x = x

idv_wild_bsig :: forall a -> a -> a
idv_wild_bsig (_ :: k) (x :: t) = x :: (t :: k)

idv_sig_shuffle :: forall a -> a -> a
idv_sig_shuffle (((t :: k1) :: k2) :: k3) x = x :: (((t :: k2) :: k1) :: k3)