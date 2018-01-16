{-# LANGUAGE DuplicateRecordFields #-}
module T11173 where
import T11173a (A(..))

-- Check that the fixity declaration applied to the field 'foo' is used
x b = b `foo` b `foo` 0
