{-# OPTIONS -fglasgow-exts #-}

--!       module Fast2haskell (
--!            Complex_type(..), Array_type(..), Assoc_type(..), Descr_type(..),
--!            abortstr, delay, fix, force, iff, iffrev, seq,
--!            pair, strcmp,
--!            entier,
--!            land_i, lnot_i, lor_i, lshift_i, rshift_i,
--!            descr,
--!            destr_update, indassoc, lowbound, tabulate, upbound, update, valassoc) where {
--	    import GlaExts;
--            import Word;
            import GHC.Base ( Double(..) )
	    -- partain fiddle
	    -- type Complex_type   = Complex Double;
            data Complex_type = CD# Double# Double#;
	    instance Eq Complex_type where {
		(CD# x y) == (CD# x2 y2) = (D# x) == (D# x2) && (D# y) == (D# y2)
	    };
	    instance Num Complex_type where {
		(CD# x y) + (CD# x2 y2)	=  ((D# x)+(D# x2)) `colon_plus` ((D# y)+(D# y2));
		(CD# x y) - (CD# x2 y2)	=  ((D# x)-(D# x2)) `colon_plus` ((D# y)-(D# y2));
		(CD# x y) * (CD# x2 y2)	=  ((D# x)*(D# x2)-(D# y)*(D# y2)) `colon_plus` ((D# x)*(D# y2)+(D# y)*(D# x2));
		negate (CD# x y)	=  negate (D# x) `colon_plus` negate (D# y);
		abs z			=  magnitude__ z `colon_plus` 0;
		signum 0		=  0;
		signum z@(CD# x y)	=  ((D# x)/r) `colon_plus` ((D# y)/r)  where { r = magnitude__ z };
		fromInteger n		=  fromInteger n `colon_plus` 0;
		fromInt n		=  fromInt n `colon_plus` 0
	    };
	    instance Show Complex_type where {
		showsPrec d (CD# a b)
		  = showParen (d > 6)
		      (showsPrec 7 (D# a) . showString " :+ " . showsPrec 7 (D# b))
	    };
#define realPart realPart__
#define imagPart imagPart__
	    realPart__ (CD# x y) = D# x;
	    imagPart__ (CD# x y) = D# y;
	    magnitude__ :: Complex_type -> Double;
	    magnitude__ (CD# x y) = magnitude ((D# x) :+ (D# y));
	    colon_plus (D# x) (D# y) = CD# x y;
	    -- end partain fiddle
            type Array_type b   = Array Int b;
            type Assoc_type a   = Assoc Int a;
            type Descr_type     = (Int,Int);
            abortstr      str                 = abort (OtherError str);
            delay         x                   = abortstr "delay not implemented";
            fix           :: (x -> x) -> x;
            fix           f                   = fix_f where {fix_f = f fix_f};
            force         x                   = x; -- error  "force not implemented";
            iff           :: Bool -> x -> x -> x;
            iff           b     x  y          = if b then x else y;
            iffrev        :: x -> x -> Bool -> x;
            iffrev        y  x      b         = if b then x else y;
            miraseq       :: x -> y -> y;
            miraseq       x    y              = seq_const y (x{-#STRICT-});
            seq_const     x    y              = x;
            pair          :: [x] -> Bool;
            pair          []                  = False;
            pair          x                   = True;
            entier        :: Double -> Double;
            entier        x                   = fromIntegral (floor x);
--!         fromInt       :: Num a => Int -> a;
--!         fromInt       i                   = fromInteger (toInteger i);
            land_i        :: Int -> Int -> Int;
            land_i        x    y              = wordToInt (bitAnd (fromInt x) (fromInt y));
            lnot_i        :: Int -> Int;
            lnot_i        x                   = wordToInt (bitCompl (fromInt x));
            lor_i         :: Int -> Int -> Int;
            lor_i         x    y              = wordToInt (bitOr (fromInt x) (fromInt y));
            lshift_i      :: Int -> Int -> Int;
            lshift_i      x    y              = wordToInt (bitLsh (fromInt x) y);
            rshift_i      :: Int -> Int -> Int;
            rshift_i      x    y              = wordToInt (bitRsh (fromInt x) y);
            write         x                   = abortstr "write not implemented";
            descr         :: Int -> Int -> Descr_type;
            descr         l    u              = (l,u);
            destr_update  :: Array_type x -> Int -> x -> Array_type x;
            destr_update  ar  i  x            = ar // [i:=x];
            indassoc      :: Assoc_type x -> Int;
            indassoc      (i:=v)              = i;
            lowbound      :: Descr_type -> Int;
            lowbound      (l,u)               = l;
            tabulate      :: (Int -> x) -> Descr_type -> Array_type x;
            tabulate      f (l,u)             = array (l,u) [i := f i | i <- [l..u]];
            upbound       :: Descr_type -> Int;
            upbound       (l,u)               = u;
            update        :: Array_type x -> Int -> x -> Array_type x;
            update        ar i x              = ar // [i:=x];
            valassoc      :: Assoc_type x -> x;
            valassoc      (i:=v)              = v;
--!       }
