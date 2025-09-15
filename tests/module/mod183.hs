{-# LANGUAGE ImportQualifiedPost #-}

-- 'qualified' can not appear in both pre and postpositive positions.

import qualified Prelude qualified

main = Prelude.undefined
