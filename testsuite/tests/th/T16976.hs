{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module T16976 where

import Language.Haskell.TH (reifyType, runIO)
import Language.Haskell.TH.Ppr (ppr_sig)
import Data.Foldable (for_)
import System.IO (hPrint, stderr)

data T s = MkT1 | MkT2

aNumber = 5
aString = "hi"

pattern P = MkT1

do  let names = [ 'aNumber, 'aString    -- local value declarations
                , 'MkT1, 'MkT2          -- local data constructor declarations
                , ''T                   -- local type constructor declarations
                , 'P                    -- local pattern synonyms
                , 'not, 'id             -- library value declarations
                , 'Nothing              -- library data constructor declarations
                , ''Maybe, ''Functor    -- library type constructor declarations
                ]
    for_ names $ \name -> do
      t <- reifyType name
      -- Why 'hPrint stderr' instead of 'print'? This is a workaround for the
      -- testsuite driver quirk, otherwise the test fails in 'ext-interp' way.
      runIO . hPrint stderr $ ppr_sig name t
    return []
