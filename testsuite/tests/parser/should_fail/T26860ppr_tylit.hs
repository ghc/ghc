{-# LANGUAGE DataKinds #-}

module T26860ppr_tylit where

import Data.Kind (Type)

-- Test that the error message containing the string literal is well-formatted.
-- See also: parser/should_fail/MultilineStringsError
type X :: Type
type X = "first line \
         \asdf\n\
         \second line"

