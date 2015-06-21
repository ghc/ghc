-- This module does not enable -XAllowDuplicateRecordFields, but it should
-- still be able to refer to non-overloaded fields like `y`

import OverloadedRecFldsRun02_A

main = print (y u)
