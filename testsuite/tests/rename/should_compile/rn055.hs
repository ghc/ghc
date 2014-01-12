{-# OPTIONS_GHC -fwarn-implicit-prelude -fwarn-unused-imports #-}
module ShouldCompile where

-- !!! should produce warnings about implicitly imported Prelude
--     (but not about the implicit import being unused)

