{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnboxedTuples #-}

module GHC.Primitive.ByteArray
  ( Array(..)
  , MutableArray(..)
  , newArray
  , readArray
  , writeArray
  , indexArray
  , indexArrayM
  , unsafeFreezeArray
  , replicateArrayP
  ) where
