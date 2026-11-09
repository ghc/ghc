{-# LANGUAGE MagicHash #-}
module EnumTagOffset (tagPlus3, tagMinus1, sucC, preC) where

import GHC.Exts

-- A constructor's index in C, its tag less one, plus or minus a literal,
-- and the next or the previous constructor loaded from C's closure table:
-- the index's - 1 folded into the literal or into the table's displacement.
data C = C0 | C1 | C2 | C3 | C4 | C5

{-# NOINLINE tagPlus3 #-}
tagPlus3 :: C -> Int
tagPlus3 c = I# (dataToTag# c +# 3#)

{-# NOINLINE tagMinus1 #-}
tagMinus1 :: C -> Int
tagMinus1 c = I# (dataToTag# c -# 1#)

{-# NOINLINE sucC #-}
sucC :: C -> C
sucC C5 = C0
sucC c = tagToEnum# (dataToTag# c +# 1#)

{-# NOINLINE preC #-}
preC :: C -> C
preC C0 = C5
preC c = tagToEnum# (dataToTag# c -# 1#)
