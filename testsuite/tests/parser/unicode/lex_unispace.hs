-- Regression tests for unicode whitespace

module ShouldCompile where

-- https://github.com/fosskers/aura/blob/cdebca1f48254ebb8286d8e38591bf644282866f/haskell/aura/lib/Aura/Languages.hs#L107
x1 = '　' -- \12288

-- https://github.com/jgm/pandoc/blob/98e77e02f6436e4b74a164762d0f3149ae7ecefa/src/Text/Pandoc/Writers/FB2.hs#L295C3-L295C32
x2 = " " -- \xa0
