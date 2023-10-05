{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PatternSynonyms #-}
module DRFPatSynExport_A where
data S = MkS { m :: Int }
pattern MkT { m } = m
