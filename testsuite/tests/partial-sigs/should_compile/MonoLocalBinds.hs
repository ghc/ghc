{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}

module MonoLocalBinds where

monoLoc :: forall a. a -> ((a, String), (a, String))
monoLoc x = (g True , g 'v')
  where
    -- g :: b -> (a, String) -- #1
    g :: b -> (a, _) -- #2
    g y = (x, "foo")

-- For #2, we should infer the same type as in #1.
