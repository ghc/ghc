{-# LANGUAGE DefaultSignatures #-}
module WildcardInDefaultSignature where


class C a where
  f :: a
  default f :: _
