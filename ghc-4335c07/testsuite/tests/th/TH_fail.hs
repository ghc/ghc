{-# LANGUAGE TemplateHaskell #-}

-- Test for sane reporting on TH code giving up.

module ShouldCompile where

$( fail "Code not written yet..." )
