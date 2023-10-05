{-# LANGUAGE TemplateHaskell #-}

module T18740d where

import Language.Haskell.TH

-- If we used 'ConE' here, then we would expect this error message:
--
--   Illegal term-level use of the type constructor ‘Bool’
--     imported from ‘Prelude’ at T18740d.hs:3:8-14
--     (and originally defined in ‘GHC.Types’)
--
-- But we used 'VarE', so the error message should say:
--
--   Illegal variable name: ‘Bool’
--
e1 = $(return (VarE ''Bool))
