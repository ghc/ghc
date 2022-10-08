{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TypeApplications #-}

module T22326_idv where

-- Definition:
idv :: forall a -> a -> a
idv (type t) x = x :: t

-- Usage:
rInt    = idv (type Int)        42
rDouble = idv (type Double)     42
rBool   = idv (type Bool)       True
rChar   = idv (type Char)       'x'
rList   = idv (type [Int])      [1,2,3]
rTup    = idv (type (Int,Bool)) (0,False)

-- Definition (using a lambda)
idv_lam :: forall a -> a -> a
idv_lam = \(type t) x -> x :: t

-- Definition (eta-reduced)
idv_eta :: forall a -> a -> a
idv_eta = idv

-- Definition (using vta on the rhs)
idv_vta :: forall a -> a -> a
idv_vta (type t) = id @t

-- Definition (using sig on the rhs)
idv_sig :: forall a -> a -> a
idv_sig (type t) = id :: t -> t

-- Definition (using a wildcard)
idv_wild :: forall a -> a -> a
idv_wild (type _) x = x

-- Usage (using a wildcard)
rBool'   = idv (type _) True
rChar'   = idv (type _) 'x'