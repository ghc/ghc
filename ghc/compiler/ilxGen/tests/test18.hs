{-# OPTIONS -fno-implicit-prelude #-}

module Test18 where

import PrelGHC
import PrelBase

eftCharFB c n x y = go x
		 where
		    go x | x ># y    = n
			 | otherwise = C# (chr# x) `c` go (x +# 1#)


eftIntFB c n x y | x ># y    = n	
		 | otherwise = go x
		 where
		   go x = I# x `c` if x ==# y then n else go (x +# 1#)

eftIntList x y | x ># y    = []
	       | otherwise = go x
	       where
		 go x = I# x : if x ==# y then [] else go (x +# 1#)


efdCharFB c n x1 x2
  | delta >=# 0# = go_up_char_fb c n x1 delta 255#
  | otherwise    = go_dn_char_fb c n x1 delta 0#
  where
    delta = x2 -# x1

efdCharList x1 x2
  | delta >=# 0# = go_up_char_list x1 delta 255#
  | otherwise    = go_dn_char_list x1 delta 0#
  where
    delta = x2 -# x1

efdtCharFB c n x1 x2 lim
  | delta >=# 0# = go_up_char_fb c n x1 delta lim
  | otherwise    = go_dn_char_fb c n x1 delta lim
  where
    delta = x2 -# x1

efdtCharList x1 x2 lim
  | delta >=# 0# = go_up_char_list x1 delta lim
  | otherwise    = go_dn_char_list x1 delta lim
  where
    delta = x2 -# x1

go_up_char_fb c n x delta lim
  = go_up x
  where
    go_up x | x ># lim  = n
	    | otherwise	= C# (chr# x) `c` go_up (x +# delta)

go_dn_char_fb c n x delta lim
  = go_dn x
  where
    go_dn x | x <# lim  = n
	    | otherwise	= C# (chr# x) `c` go_dn (x +# delta)

go_up_char_list x delta lim
  = go_up x
  where
    go_up x | x ># lim  = []
	    | otherwise	= C# (chr# x) : go_up (x +# delta)


go_dn_char_list x delta lim
  = go_dn x
  where
    go_dn x | x <# lim  = []
	    | otherwise	= C# (chr# x) : go_dn (x +# delta)

efdtIntFB c n x1 x2 y
  | delta >=# 0# = if x1 ># y then n else go_up_int_fb c n x1 delta lim
  | otherwise    = if x1 <# y then n else go_dn_int_fb c n x1 delta lim 
  where
    delta = x2 -# x1
    lim   = y -# delta

efdtIntList x1 x2 y
  | delta >=# 0# = if x1 ># y then [] else go_up_int_list x1 delta lim
  | otherwise    = if x1 <# y then [] else go_dn_int_list x1 delta lim
  where
    delta = x2 -# x1
    lim   = y -# delta

efdIntFB c n x1 x2
  | delta >=# 0# = go_up_int_fb c n x1 delta (  2147483647#  -# delta)
  | otherwise    = go_dn_int_fb c n x1 delta ((-2147483648#) -# delta)
  where
    delta = x2 -# x1

efdIntList x1 x2
  | delta >=# 0# = go_up_int_list x1 delta (  2147483647#  -# delta)
  | otherwise    = go_dn_int_list x1 delta ((-2147483648#) -# delta)
  where
    delta = x2 -# x1

-- In all of these, the (x +# delta) is guaranteed not to overflow

go_up_int_fb c n x delta lim
  = go_up x
  where
    go_up x | x ># lim  = I# x `c` n
	    | otherwise = I# x `c` go_up (x +# delta)

go_dn_int_fb c n x delta lim 
  = go_dn x
  where
    go_dn x | x <# lim  = I# x `c` n
	    | otherwise = I# x `c` go_dn (x +# delta)

go_up_int_list x delta lim
  = go_up x
  where
    go_up x | x ># lim  = [I# x]
	    | otherwise = I# x : go_up (x +# delta)

go_dn_int_list x delta lim 
  = go_dn x
  where
    go_dn x | x <# lim  = [I# x]
	    | otherwise = I# x : go_dn (x +# delta)
eftInt 	= eftIntList
efdInt 	= efdIntList
efdtInt = efdtIntList


