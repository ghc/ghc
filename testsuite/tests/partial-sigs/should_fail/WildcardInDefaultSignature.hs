{-# LANGUAGE DefaultSignatures #-}
module WildcardInDefaultSignature where

class C a where default f :: _
