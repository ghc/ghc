-- Trac #2506

module ShouldCompile where
import Control.Exception (assert)

foo = True `assert` ()
