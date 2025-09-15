{-# OPTIONS_GHC -Werror=unused-imports #-}

module T23557 (main) where

import T23557_aux (foo)

main :: IO ()
main = print foo

-- We should not get an unused import for the import of the field selector "foo",
-- because the module we are importing from uses NoFieldSelectors.
