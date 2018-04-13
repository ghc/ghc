{-# LANGUAGE NoImplicitPrelude #-}
module Dollar where
{-
inplace/bin/ghc-stage1 -O2 -dcore-lint

See in Control.Arrow and Data.Functor.Contravariant
-}

import GHC.Base

class Or p where
	or :: p a b -> p a b -> p a b

instance Or (->) where
	or x _ = x


foo = or Just (\x -> Just x)
