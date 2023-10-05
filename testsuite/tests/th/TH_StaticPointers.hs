{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StaticPointers #-}

-- | A test of static forms in TH quotations.
module Main(main) where

import GHC.StaticPtr

main = print $ deRefStaticPtr $([| static g :: StaticPtr String |])

g = "found"
