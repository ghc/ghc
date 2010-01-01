{-# LANGUAGE NewQualifiedOperators #-}
-- tests for new qualified operator syntax

module ShouldFail where

add x y = x `Prelude.+` y
