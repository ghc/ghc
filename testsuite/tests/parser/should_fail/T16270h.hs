{-# LANGUAGE GHC2021 #-}
-- We can't test module header parsing errors using the same file as other
-- parsing errors (in ../T16270.hs) because HeaderInfo.getImports fails fast
-- on parsing imports:
--
--      if errorsFound dflags ms
--        then throwIO $ mkSrcErr errs
--
module T16270h (type G) where

import "pkg?" M
import "pkg!" M

data G a
