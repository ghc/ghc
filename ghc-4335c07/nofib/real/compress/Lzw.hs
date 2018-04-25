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

#if defined(__GLASGOW_HASKELL__) && defined(SLEAZY_UNBOXING)
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

#else {- ! __GLASGOW_HASKELL__ -}

#define FAST_INT Int
#define ILIT(x) (x)
#define IBOX(x) (x)
#define _ADD_ +
#define _SUB_ -
#define _MUL_ *
#define _DIV_ `div`
#define _QUOT_ `quot`
#define _REM_ `rem`
#define _NEG_ -
#define _EQ_ ==
#define _LT_ <
#define _LE_ <=
#define _GE_ >=
#define _GT_ >
#define _CHR_ toEnum

#define FAST_BOOL Bool
#define _TRUE_ True
#define _FALSE_ False
#define _IS_TRUE_(x) (x)

#define FAST_CHAR Char
#define CBOX(x) (x)

type FAST_TRIPLE = ([Char], Int, PrefixTree);
#define _TRIP_(a,b,c) ((a), (b), (c))

data PrefixElement = PTE FAST_CHAR FAST_INT PrefixTree;
#define _PTE_(a,b,c) (PTE (a) (b) (c))

#endif  {- ! __GLASGOW_HASKELL__ -}

-- end of partain

data PrefixTree = PTNil | PT PrefixElement PrefixTree PrefixTree;

--create_code_table :: PrefixTree; -- partain: sig

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
      case (code_string input ILIT(0) next_code code_table) of {
	_TRIP_(input2,n,code_table2) ->
	    IBOX(n) : lzw_code_file input2 code_table2 (next_code _ADD_ ILIT(1))
      };

code_string :: [Char] -> FAST_INT -> FAST_INT -> PrefixTree -> FAST_TRIPLE;

#if defined(__GLASGOW_HASKELL__) && defined(SLEAZY_UNBOXING)
code_string input@(CBOX(c) : input2) old_code next_code (PT k v t {-p@(PTE k v t)-} l r)
    | CBOX(c) <  CBOX(k) = f1 r1 {-p-} k v t r
    | CBOX(c) >  CBOX(k) = f2 r2 {-p-} k v t l
    | otherwise {- CBOX(c) == CBOX(k) -} = f3 r3 k v l r
#else
code_string input@(CBOX(c) : input2) old_code next_code (PT p@(PTE k v t) l r)
    | CBOX(c) <  CBOX(k) = f1 r1 p r
    | CBOX(c) >  CBOX(k) = f2 r2 p l
    | otherwise {- CBOX(c) == CBOX(k) -} = f3 r3 k v l r
#endif
    where {
        r1 = code_string input old_code next_code l;
        r2 = code_string input old_code next_code r;
        r3 = code_string input2 v next_code t;

#if defined(__GLASGOW_HASKELL__) && defined(SLEAZY_UNBOXING)
        f1 _TRIP_(input_l,nl,l2) k v t r   = _TRIP_(input_l,nl,PT k v t l2 r);
        f2 _TRIP_(input_r,nr,r2) k v t l   = _TRIP_(input_r,nr,PT k v t l r2);
#else
        f1 _TRIP_(input_l,nl,l2) p r   = _TRIP_(input_l,nl,PT p l2 r);
        f2 _TRIP_(input_r,nr,r2) p l   = _TRIP_(input_r,nr,PT p l r2);
#endif
        f3 _TRIP_(input2,n,t2) k v l r = _TRIP_(input2, n, PT _PTE_(k, v, t2) l r);
    };

--code_string input@(c : input2) old_code next_code (PT p@(PTE k v t) l r)
--  | c < k = (input_l,nl,PT p l' r)
--  | c > k = (input_r,nr,PT p l r')
--  | c == k = (input',n,PT (PTE k v t') l r)
--  where {
--      (input_l,nl,l') = code_string input old_code next_code l;
--      (input_r,nr,r') = code_string input old_code next_code r;
--      (input',n,t') = code_string input2 v next_code t;
--  };

code_string input@(CBOX(c) : input_file2) old_code next_code PTNil
    =   if (next_code _GE_ ILIT(4096)) 
        then _TRIP_(input, old_code, PTNil)
        else _TRIP_(input, old_code, PT _PTE_(c, next_code, PTNil) PTNil PTNil);

code_string [] old_code next_code code_table = _TRIP_([], old_code, PTNil);

integer_list_to_char_list (IBOX(n) : l)
    =   CBOX(_CHR_ (n _QUOT_ ILIT(16))) : integer_list_to_char_list2 l n;
integer_list_to_char_list [] = [];

    integer_list_to_char_list2 (IBOX(c) : l) n
        =   CBOX(_CHR_ ((n _MUL_ ILIT(16)) _ADD_ ((c _QUOT_ ILIT(256)) _REM_ ILIT(16))))
	    : CBOX(_CHR_ c)
            : integer_list_to_char_list l;
    integer_list_to_char_list2 [] n = CBOX(_CHR_ (n _MUL_ ILIT(16))) : [];

main :: IO ();
main = getContents >>= \ input_string -> main2 input_string;

main2 :: String -> IO ();
main2 input_string
    = putStr output_list
    where {
        output_list = integer_list_to_char_list code_list;
        code_list = lzw_code_file input_string create_code_table ILIT(256);
    };

}
