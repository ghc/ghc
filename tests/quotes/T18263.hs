{-# LANGUAGE TemplateHaskell #-}

{-
This is to test that we don't get the error:

    The exact Name ‘x_aFi’ is not in scope
        Probable cause: you used a unique Template Haskell name (NameU),
        perhaps via newName, but did not bind it
        If that's it, then -ddump-splices might be useful

When looking up something with 'lookupGlobalOccRn_maybe', which is called by
'lookupThName'. This can happen when using a gensymmed name via newName.

This should still fail to compile though, as reify should complain that "x"
isn't in the type environment, albeit with one less error.

Later (March 2021): actually this should really compile fine:
  * The [d| ... |] splices in a top-level binding for x
  * The reify looks for that binding
It was really a bug that it didn't work, now fixed.

-}

module T18263 where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import System.IO

do
  n <- newName "x"
  addModFinalizer $ reify n >>= runIO . hPrint stderr
  [d| $(varP n) = 42 |]
