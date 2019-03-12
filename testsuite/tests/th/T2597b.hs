{-# LANGUAGE TemplateHaskell #-}

-- Test #2597 (second bug)

module ShouldCompile where
import T2597b_Lib

bug2 = $mkBug2

