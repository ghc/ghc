{-# LANGUAGE PartialTypeSignatures, NamedWildCards #-}

module ScopedNamedWildcards where

test3 :: _
test3 x = const (let x :: _b
                     x = True in False) $
          const (let x :: _b
                     x = 'a' in True) $
          not x
