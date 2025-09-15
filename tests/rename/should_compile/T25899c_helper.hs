{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE PatternSynonyms #-}

module T25899c_helper
  ( type T (data P, data f)
  ) where

data T = MkT Int

pattern P {f} = MkT f
