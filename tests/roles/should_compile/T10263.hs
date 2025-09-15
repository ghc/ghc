{-# LANGUAGE RoleAnnotations #-}
module T10263 where

data Maybe a = AF
type role Maybe representational
