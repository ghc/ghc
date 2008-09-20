{-# LANGUAGE TemplateHaskell #-}

-- Test Trac #2597 (first bug)

module ShouldCompile where
import T2597a_Lib

bug = $mkBug
