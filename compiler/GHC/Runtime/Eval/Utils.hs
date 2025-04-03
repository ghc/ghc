module GHC.Runtime.Eval.Utils where

import GHC.Prelude
import Data.Char
import Data.List (elemIndices)

-- | Split up a string with an eventually qualified declaration name into 3 components
--
--   1. module name
--   2. top-level decl
--   3. full-name of the eventually nested decl, but without module qualification
--
-- === __Example__
--
-- @
--     "foo"           = ("", "foo", "foo")
--     "A.B.C.foo"     = ("A.B.C", "foo", "foo")
--     "M.N.foo.bar"   = ("M.N", "foo", "foo.bar")
-- @
splitIdent :: String -> (String, String, String)
splitIdent [] = ("", "", "")
splitIdent inp@(a : _)
    | (isUpper a) = case fixs of
        []            -> (inp, "", "")
        (i1 : [] )    -> (upto i1, from i1, from i1)
        (i1 : i2 : _) -> (upto i1, take (i2 - i1 - 1) (from i1), from i1)
    | otherwise = case ixs of
        []            -> ("", inp, inp)
        (i1 : _)      -> ("", upto i1, inp)
  where
    ixs = elemIndices '.' inp        -- indices of '.' in whole input
    fixs = dropWhile isNextUc ixs    -- indices of '.' in function names              --
    isNextUc ix = isUpper $ safeInp !! (ix+1)
    safeInp = inp ++ " "
    upto i = take i inp
    from i = drop (i + 1) inp

-- | Qualify an identifier name with a module name
--
-- @
-- combineModIdent "A" "foo"  =  "A.foo"
-- combineModIdent ""  "foo"  =  "foo"
-- @
combineModIdent :: String -> String -> String
combineModIdent mod ident
          | null mod   = ident
          | null ident = mod
          | otherwise  = mod ++ "." ++ ident
