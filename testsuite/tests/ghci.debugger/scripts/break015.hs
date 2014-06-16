f i = if ?flag then i*2 else i

g i = let ?flag=False in f i
