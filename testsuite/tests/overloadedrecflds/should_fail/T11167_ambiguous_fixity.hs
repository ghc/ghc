{-# LANGUAGE DuplicateRecordFields #-}
module T11167_ambiguous_fixity where
import T11167_ambiguous_fixity_A
import T11167_ambiguous_fixity_B

x a = (a :: A) `foo` 0
