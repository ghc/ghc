{-# LANGUAGE LinearTypes #-}
module LinearErrOrigin where

-- The error message should mention "arising from multiplicity of x".

foo :: (a %p -> b) -> a %q -> b
foo f x = f x
