{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}

module T8507 where

type Stringy1 a = (Read a, Show a)

$([d|type Stringy2 a = (Read a, Show a) |])
