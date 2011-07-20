-- !!! Special Ids and ops

-- The special ids 'as', 'qualified' and 'hiding' should be 
-- OK in both qualified and unqualified form.
-- Ditto special ops

module ShouldCompile where
import Prelude hiding ( (-) )

as	  = ShouldCompile.as
hiding	  = ShouldCompile.hiding
qualified = ShouldCompile.qualified
x!y	  = x ShouldCompile.! y
x-y	  = x ShouldCompile.- y
