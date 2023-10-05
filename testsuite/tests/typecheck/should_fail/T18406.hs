{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts #-}

module Bug where

class C a b | a -> b where
  op :: a -> b -> ()

f x = op True x

{- We could accept this, quantifying over a C Bool b constraint. But this is a
bit silly, actually, because the b is fixed by the fundep. We don't know what
it's fix to, but it's definitely fixed. So, in the end, we choose not to
Henry Ford polymorphism ("it works for any b as long as b is ???") and not
to quantify. Users can quantify manually if they want.
-}
