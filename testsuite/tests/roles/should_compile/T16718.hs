{-# LANGUAGE RoleAnnotations, TemplateHaskell #-}

module T16718 where

$([d| type role P phantom
      data P a
    |])
