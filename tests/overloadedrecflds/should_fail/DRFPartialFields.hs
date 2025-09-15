{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Werror=partial-fields #-}
module DRFPartialFields where
data T = MkT1 { foo :: Int } | MkT2
