{-# LANGUAGE NoImplicitPrelude, TypeFamilies #-}

module T5853 where
import Prelude (undefined,Bool(..),Show(..),(.))

type family Elem f :: *
type family Subst f b :: *

class (Subst fa (Elem fa) ~ fa) => F fa where
	(<$>) :: (Elem fa ~ a, Elem fb ~ b,
	          Subst fa b ~ fb, Subst fb a ~ fa) =>
			 (a -> b) -> (fa -> fb)

{-# RULES
 "map/map" forall f g xs. f <$> (g <$> xs) = (f.g) <$> xs
	#-} 
