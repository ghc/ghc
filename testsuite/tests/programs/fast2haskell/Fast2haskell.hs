       module Fast2haskell (
            Complex_type, Array_type, Assoc_type, Descr_type,
            abortstr, delay, fix, force, iff, iffrev, seQ,
            pair, strcmp,
            entier,
            land_i, lnot_i, lor_i, lshift_i, rshift_i,
            descr,
            destr_update, indassoc, lowbound, tabulate, upbound, update, valassoc) where {
	    import Data.Bits;
--            import Word2;
            import Data.Word;
	    import Data.Complex; -- 1.3
	    import Data.Array; -- 1.3
--	    import Data.Int ( Num(fromInt) );
            type Complex_type   = Complex Double;
            type Array_type b   = Array Int b;
            type Assoc_type a   = (Int, a);
            type Descr_type     = (Int,Int);
            abortstr      str                 = error ("abort:"++str); -- abort (OtherError str);
            delay         x                   = abortstr "delay not implemented";
            fix           f                   = fix_f where {fix_f = f fix_f};
            force         x                   = x; -- error  "force not implemented";
            iff           b     x  y          = if b then x else y;
            iffrev        y  x      b         = if b then x else y;
            seQ           x    y              = x `seq` y;
            pair          []                  = False;
            pair          x                   = True;
            strcmp        :: [Char] -> [Char] -> Bool;
            strcmp        x      y            = x == y;
            entier        x                   = fromIntegral (floor x);
            land_i        :: Int -> Int -> Int;
            land_i        x    y              = x .&. y;
            lnot_i        :: Int -> Int;
            lnot_i        x                   = complement x;
            lor_i         :: Int -> Int -> Int;
            lor_i         x    y              = x .|. y;
            lshift_i      :: Int -> Int -> Int;
            lshift_i      x    y              = x `shiftL` y;
            rshift_i      :: Int -> Int -> Int;
            rshift_i      x    y              = x `shiftR` y;
            write         x                   = abortstr "write not implemented";
            descr         l    u              = (l,u);
            destr_update  ar  i  x            = ar // [(i,x)];
            indassoc      (i,v)              = i;
            lowbound      (l,u)               = l;
            tabulate      f (l,u)             = listArray (l,u) [f i | i <- [l..u]];
            upbound       (l,u)               = u;
            update        ar i x              = ar // [(i,x)];
            valassoc      (i,v)               = v;
       }
