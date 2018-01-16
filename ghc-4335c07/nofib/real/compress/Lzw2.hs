--	Lzw2.hs looks like an earlier version of Lzw.hs

module Main (main){-export list added by partain-} where {

-- partain: with "ghc -cpp -DSLEAZY_UNBOXING", you get (guess what)?
-- without it, you get the code as originally written.
--
-- Things done here:
--  * The obvious unboxing (e.g., Int ==> Int#).
--  * use quot/rem, not div/mod
--  * inline PrefixElement type into PrefixTree.PT constructor
--  * cvt final clause of 3-way comparison to "otherwise"
--  * use shifts, not quot/rem (not necessary: C compiler converts
--    them just fine)
--
-- Obviously, more egregious hacking could be done:
--  * replace Tuple/List types that mention Ints with specialised
--    variants

#define FAST_INT Int#
#define ILIT(x) (x#)
#define IBOX(x) (I# (x))
#define _ADD_ `plusInt#`
#define _SUB_ `minusInt#`
#define _MUL_ `timesInt#`
#define _DIV_ `divInt#`
#define _QUOT_ `quotInt#`
#define _REM_ `remInt#`
#define _NEG_ negateInt#
#define _EQ_ `eqInt#`
#define _LT_ `ltInt#`
#define _LE_ `leInt#`
#define _GE_ `geInt#`
#define _GT_ `gtInt#`
#define _CHR_ chr#

#define FAST_BOOL Int#
#define _TRUE_ 1#
#define _FALSE_ 0#
#define _IS_TRUE_(x) ((x) `eqInt#` 1#)

#define FAST_CHAR Char#
#define CBOX(x) (C# (x))

data FAST_TRIPLE = TRIP [Char] Int# PrefixTree;
#define _TRIP_(a,b,c) (TRIP (a) (b) (c))

#define PrefixElement FAST_CHAR FAST_INT PrefixTree
#define _PTE_(a,b,c) (a) (b) (c)

-- end of partain

data PrefixTree = PTNil | PT PrefixElement PrefixTree PrefixTree;

create_code_table = create_code_table2 ILIT(0) ILIT(256);

    create_code_table2 :: FAST_INT -> FAST_INT -> PrefixTree;
    create_code_table2 first_code ILIT(0) = PTNil;
    create_code_table2 first_code ILIT(1)
    =   PT _PTE_((_CHR_ first_code), first_code, PTNil) PTNil PTNil;
    create_code_table2 first_code n_codes
    =   PT _PTE_((_CHR_ m_code), m_code, PTNil) left right
    where {
        left = create_code_table2 first_code (m_code _SUB_ first_code);
        right = create_code_table2 m_code2 ((first_code _ADD_ n_codes) _SUB_ m_code2);
        m_code = (first_code _ADD_ (first_code _ADD_ n_codes _SUB_ ILIT(1))) _QUOT_ ILIT(2);
        m_code2 = m_code _ADD_ ILIT(1);
    };

lzw_code_file :: [Char] -> PrefixTree -> FAST_INT -> [Int];
lzw_code_file [] code_table next_code = [];
lzw_code_file input code_table next_code
    =   -- partain: case-ified lazy where
      case (code_string ILIT(0) next_code input code_table) of {
	_TRIP_(input2,n,code_table2) ->
	    IBOX(n) : lzw_code_file input2 code_table2 (next_code _ADD_ ILIT(1))
      };

code_string :: FAST_INT -> FAST_INT -> [Char] -> PrefixTree -> FAST_TRIPLE;

code_string old_code next_code input@(CBOX(c) : input2) (PT k v t {-p@(PTE k v t)-} l r)
    | CBOX(c) <  CBOX(k) = {-# SCC "cs1" #-} (f1 r1 {-p-} k v t r)
    | CBOX(c) >  CBOX(k) = {-# SCC "cs2" #-} (f2 r2 {-p-} k v t l)
    | otherwise {- CBOX(c) == CBOX(k) -} = {-# SCC "cs3" #-} (f3 r3 k v l r)
    where {
        r1 = code_string old_code next_code input  l;
        r2 = code_string old_code next_code input  r;
        r3 = code_string v        next_code input2 t;

        f1 _TRIP_(input_l,nl,l2) k v t r   = _TRIP_(input_l,nl,PT k v t l2 r);
        f2 _TRIP_(input_r,nr,r2) k v t l   = _TRIP_(input_r,nr,PT k v t l r2);
        f3 _TRIP_(input2,n,t2) k v l r = _TRIP_(input2, n, PT _PTE_(k, v, t2) l r);
    };

code_string old_code next_code input@(CBOX(c) : input_file2) PTNil
    =   if (next_code _GE_ ILIT(4096)) 
        then {-# SCC "cs4" #-} _TRIP_(input, old_code, PTNil)
        else {-# SCC "cs5" #-} _TRIP_(input, old_code, PT _PTE_(c, next_code, PTNil) PTNil PTNil);

code_string old_code next_code [] code_table = {-# SCC "cs6" #-} _TRIP_([], old_code, PTNil);

integer_list_to_char_list (IBOX(n) : l)
    =   CBOX(_CHR_ (n _QUOT_ ILIT(16))) : integer_list_to_char_list2 l n;
integer_list_to_char_list [] = [];

    integer_list_to_char_list2 (IBOX(c) : l) n
        =   CBOX(_CHR_ ((n _MUL_ ILIT(16)) _ADD_ ((c _QUOT_ ILIT(256)) _REM_ ILIT(16))))
	    : CBOX(_CHR_ c)
            : integer_list_to_char_list l;
    integer_list_to_char_list2 [] n = CBOX(_CHR_ (n _MUL_ ILIT(16))) : [];

main :: IO ();
main = getContents >>= \input_string -> main2 input_string;

main2 :: String -> IO ();
main2 input_string
    = putStr output_list
    where {
        output_list = integer_list_to_char_list code_list;
        code_list = lzw_code_file input_string create_code_table ILIT(256);
    };

}
