{-# LANGUAGE TemplateHaskell #-}
-- Trac #2931

module Foo where
a = 1

-- NB: no newline after the 'a'!
b = 'a