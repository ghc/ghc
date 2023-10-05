{-# LANGUAGE DuplicateRecordFields #-}

module QualifiedRecordUpdate where

import           QualifiedRecordUpdate_aux      ( R(fld1, fld2), S(fld1, fld2) )
import qualified QualifiedRecordUpdate_aux as B ( R(fld1, fld2), S(fld1) )

-- Unambiguous record update: the only record datatype in the B namespace
-- which contains field fld2 is R.
f r = r { B.fld1 = 3, B.fld2 = False }
