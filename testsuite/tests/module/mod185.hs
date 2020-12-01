{-# LANGUAGE ImportQualifiedPost #-}
-- The span of the import decl should include the 'qualified' keyword.
import Prelude qualified

main = Prelude.undefined
