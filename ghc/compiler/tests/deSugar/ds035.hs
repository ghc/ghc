data CList = CNil | CCons Int# CList

mk :: Int# -> CList
mk n = case (n ==# 0#) of
       0# -> CNil
       _  -> CCons 1# (mk (n `minusInt#` 1#))

clen :: CList -> Int#
clen CNil = 0#
clen (CCons _ cl) = 1# +# (clen cl)

main = case len4_twice of
	    8# -> "bingo\n"
	    _  -> "oops\n"
  where
    list4	= mk 4#
    len4	= clen list4
    len4_twice	= len4 +# len4
