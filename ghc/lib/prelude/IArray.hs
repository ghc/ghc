-- *** all of PreludeArray except the actual data decls

module PreludeArray (
	Array, Assoc,

	(!),
	(//),
	accum,
	accumArray,
	amap,
	array,
	assocs,
	bounds,
	elems,
	indices,
	ixmap,
	listArray,
	_arrEleBottom,
	_newArray,
	_freezeArray
    ) where

import Cls
import Core
import IChar
import IInt		-- instances
import IDouble
import IList
import ITup2
import List		( (++), zipWith, foldr )
import Prel		( (&&), (.) )
import PS		( _PackedString, _unpackPS )
import Text
import TyArray   	( Array(..), Assoc(..) )
import TyComplex
import PreludeGlaST

-- Hey! This isn't wimp Haskell-report code!  This is
-- the Business End of Arrays...

--infixl 9  !
--infixl 9  //
--infix	 1  :=

-----------------------------------------------------------
instance (Eq a, Eq b) => Eq (Assoc a b) where
    (a1 := b1) == (a2 := b2) = a1 == a2 && b1 == b2
    a /= b 	     	     = if a == b then False else True

instance (Ord a, Ord b) => Ord (Assoc a b) where
    a <	 b  = case _tagCmp a b of { _LT -> True;  _EQ -> False; _GT -> False }
    a <= b  = case _tagCmp a b of { _LT -> True;  _EQ -> True;  _GT -> False }
    a >= b  = case _tagCmp a b of { _LT -> False; _EQ -> True;  _GT -> True  }
    a >	 b  = case _tagCmp a b of { _LT -> False; _EQ -> False; _GT -> True  }
    max a b = case _tagCmp a b of { _LT -> b; _EQ -> a;  _GT -> a }
    min a b = case _tagCmp a b of { _LT -> a; _EQ -> a;  _GT -> b }
    _tagCmp (a1 := b1) (a2 := b2)					
      = case (_tagCmp a1 a2) of { _LT -> _LT; _GT -> _GT; _EQ -> _tagCmp b1 b2 }

instance (Ix a, Ix b) => Ix (Assoc a b) where
    range (l1 := l2, u1 := u2)			
      = [ (i1 := i2) | i1 <- range (l1, u1), i2 <- range (l2, u2) ]

    index (l1 := l2, u1 := u2) (i1 := i2)
      = index (l1, u1) i1 * (index (l2, u2) u2 + 1){-rangeSize (l2, u2)-} + index (l2, u2) i2

    inRange (l1 := l2, u1 := u2) (i1 := i2)
      = inRange (l1, u1) i1 && inRange (l2, u2) i2

instance (Text a, Text b) => Text (Assoc a b) where
    -- magic fixity wired in: infix 1 :=
    readsPrec p
      = readParen ( p > 1 )
	  (\ r -> [ (x := y, s2) | (x,    s0) <- readsPrec 2 r,
				   (":=", s1) <- lex s0,
				   (y,    s2) <- readsPrec 2 s1 ])
    showsPrec d (a := b)
      = showParen (d > 1)
	  (showsPrec 2 a . showString " := " . showsPrec 2 b)

    readList = _readList (readsPrec 0)
    showList = _showList (showsPrec 0)

-- ToDo: *** Binary

-----------------------------------------------------------

type IPr = (Int, Int)

{-# GENERATE_SPECS array a{~,Int,IPr} b{} #-}
array	    :: (Ix a) => (a,a) -> [Assoc a b] -> Array a b

{-# GENERATE_SPECS (!) a{~,Int,IPr} b{} #-}
(!)	    :: (Ix a) => Array a b -> a -> b

bounds	    :: Array a b -> (a,a)

{-# GENERATE_SPECS listArray a{~,Int,IPr} b{} #-}
listArray   :: (Ix a) => (a,a) -> [b] -> Array a b

{-# GENERATE_SPECS indices a{~,Int,IPr} b{} #-}
indices	    :: (Ix a) => Array a b -> [a]

{-# GENERATE_SPECS elems a{~,Int,IPr} b{} #-}
elems	    :: (Ix a) => Array a b -> [b]

{-# GENERATE_SPECS assocs a{~,Int,IPr} b{} #-}
assocs	    :: (Ix a) => Array a b -> [Assoc a b]

{-# GENERATE_SPECS accumArray a{~,Int,IPr} b{} c{} #-}
accumArray  :: (Ix a) => (b -> c -> b) -> b -> (a,a) -> [Assoc a c] -> Array a b

{-# GENERATE_SPECS (//) a{~,Int,IPr} b{} #-}
(//)	    :: (Ix a) => Array a b -> [Assoc a b] -> Array a b

{-# GENERATE_SPECS accum a{~,Int,IPr} b{} c{} #-}
accum	    :: (Ix a) => (b -> c -> b) -> Array a b -> [Assoc a c] -> Array a b

{-# GENERATE_SPECS amap a{~,Int,IPr} b{} c{} #-}
amap	    :: (Ix a) => (b -> c) -> Array a b -> Array a c

ixmap	    :: (Ix a, Ix b) => (a,a) -> (a -> b) -> Array b c -> Array a c


{- "array", "!" and "bounds" are basic;
   the rest can be defined in terms of them
-}

bounds (_Array b _)  = b

#ifdef USE_FOLDR_BUILD
{-# INLINE array #-}
#endif
array ixs@(ix_start, ix_end) ivs =
   _runST ( \ s ->
	case _newArray ixs _arrEleBottom s 		of { (arr@(_MutableArray _ arr#),s) ->
	let
         fill_one_in (S# s#) (i := v)
             = case index ixs  i		    	of { I# n# ->
		case writeArray# arr# n# v s# 		of { s2# -> S# s2# }}
	in
	case foldl fill_one_in s ivs 			of { s@(S# _) -> 
	_freezeArray arr s }})

_arrEleBottom = error "(!){PreludeArray}: undefined array element"

{- OLD:
array ixs@(ix_start, ix_end) ivs
  = _runST (
	newArray ixs arrEleBottom	`thenStrictlyST` \ arr# ->
	fill_it_in arr# ivs		`seqStrictlyST`
	freezeArray arr#
    )
  where
    arrEleBottom = error "(!){PreludeArray}: undefined array element"
-}

(_Array bounds arr#) ! i
  = let n# = case (index bounds i) of { I# x -> x } -- index fails if out of range
    in
    case (indexArray# arr# n#) of
      _Lift v -> v

fill_it_in arr lst s
  = foldr fill_one_in (returnStrictlyST ()) lst s
  where  -- **** STRICT **** (but that's OK...)
    fill_one_in (i := v) rst s
      = (writeArray arr i v `seqStrictlyST` rst) s

{- the rest ------------------------------------------------- -}

listArray b vs	      = array b (zipWith (:=) (range b) vs)

#ifdef USE_FOLDR_BUILD
{-# INLINE indices #-}
{-# INLINE elems #-}
{-# INLINE assocs #-}
#endif

indices a	      = range (bounds a)

elems a		      = [a!i | i <- indices a]

assocs a	      = [i := a!i | i <- indices a]

#ifdef USE_REPORT_PRELUDE
a // us		      = array (bounds a)
			    ([i := a!i | i <- indices a \\ [i | i:=_ <- us]]
			     ++ us)

accum f		      = foldl (\a (i := v) -> a // [i := f (a!i) v])

accumArray f z b      = accum f (array b [i := z | i <- range b])

#else /* ! USE_REPORT_PRELUDE */

-- TODO: add (//), accum, accumArray, listArray

old_array // ivs
  = _runST (
	-- copy the old array:
	thawArray old_array		    `thenStrictlyST` \ arr# ->	
	-- now write the new elements into the new array:
	fill_it_in arr# ivs		    `seqStrictlyST`
	freezeArray arr#
    )
  where
    bottom = error "(//){PreludeArray}: error in copying old array\n"

-- zap_with_f: reads an elem out first, then uses "f" on that and the new value

zap_with_f f arr lst s
  = foldr zap_one (returnStrictlyST ()) lst s
  where
    zap_one (i := new_v) rst s
      = (readArray  arr i		 `thenStrictlyST`  \ old_v ->
	writeArray arr i (f old_v new_v) `seqStrictlyST`
	rst) s

accum f arr ivs
  = _runST (
	-- copy the old array:
	newArray (bounds arr) bottom	`thenST` \ arr# ->
	fill_it_in arr# (assocs arr)	`seqST`

	-- now zap the elements in question with "f":
	zap_with_f f arr# ivs		`seqST`
	freezeArray arr#
    )
  where
    bottom = error "accum{PreludeArray}: error in copying old array\n"

accumArray f zero ixs ivs
  = _runST (
	newArray ixs zero	`thenST` \ arr# ->
	zap_with_f f  arr# ivs	`seqST`
	freezeArray arr#
    )
#endif /* ! USE_REPORT_PRELUDE */

amap f a	      = array b [i := f (a!i) | i <- range b]
			where b = bounds a

ixmap b f a	      = array b [i := a ! f i | i <- range b]

instance (Ix a, Eq b)	=> Eq (Array a b)  where 
    a == a'  =  assocs a == assocs a'
    a /= a'  =  assocs a /= assocs a'

instance (Ix a, Ord b) => Ord (Array a b) where
    a <	 b  = case _tagCmp a b of { _LT -> True;  _EQ -> False; _GT -> False }
    a <= b  = case _tagCmp a b of { _LT -> True;  _EQ -> True;  _GT -> False }
    a >= b  = case _tagCmp a b of { _LT -> False; _EQ -> True;  _GT -> True  }
    a >	 b  = case _tagCmp a b of { _LT -> False; _EQ -> False; _GT -> True  }

    max a b = case _tagCmp a b of { _LT -> b; _EQ -> a;  _GT -> a }
    min a b = case _tagCmp a b of { _LT -> a; _EQ -> a;  _GT -> b }

    _tagCmp a b = _tagCmp (assocs a) (assocs b)

instance  (Ix a, Text a, Text b) => Text (Array a b)  where
    showsPrec p a = showParen (p > 9) (
		    showString "array " .
		    showsPrec 0 (bounds a) . showChar ' ' .
		    showList (assocs a) )

    readsPrec p = readParen (p > 9)
	   (\r -> [(array b as, u) | ("array",s) <- lex r,
				     (b,t)	 <- readsPrec 0 s,
				     (as,u)	 <- readList t ]
		  ++
		  [(listArray b xs, u) | ("listArray",s) <- lex r,
					 (b,t)		 <- readsPrec 0 s,
					 (xs,u)		 <- readList t ])

    readList = _readList (readsPrec 0)
    showList = _showList (showsPrec 0)


{-# SPECIALIZE instance Text (Array Int Double) #-}
{-# SPECIALIZE instance Text (Array (Int,Int) Double) #-}

{- **** OMITTED **** (ToDo)
instance  (Ix a, Binary a, Binary b) => Binary (Array a b)  where
    showBin a = showBin (bounds a) . showBin (elems a)

    readBin bin = (listArray b vs, bin'')
		 where (b,bin')	  = readBin bin
		       (vs,bin'') = readBin bin'
-}
{- ToDo ...

#if defined(__UNBOXED_INSTANCES__)

-- {-# GENERATE_SPECS array a{~,Int#,Int,IPr} b{Int#,Double#} #-}
-- {-# GENERATE_SPECS (!) a{~,Int#,Int,IPr} b{Int#,Double#} #-}
-- {-# GENERATE_SPECS bounds a{~,Int#} b{Int#,Double#} #-}
-- {-# GENERATE_SPECS listArray a{~,Int#,Int,IPr} b{Int#,Double#} #-}
-- {-# GENERATE_SPECS indices a{~,Int#,Int,IPr} b{Int#,Double#} #-}
-- {-# GENERATE_SPECS elems a{~,Int#,Int,IPr} b{Int#,Double#} #-}
-- {-# GENERATE_SPECS assocs a{~,Int#,Int,IPr} b{Int#,Double#} #-}
-- {-# GENERATE_SPECS accumArray a{~,Int#,Int,IPr} b{Int#,Double#} c{Int#,Double#} #-}
-- {-# GENERATE_SPECS (//) a{~,Int#,Int,IPr} b{Int#,Double#} #-}
-- {-# GENERATE_SPECS accum a{~,Int#,Int,IPr} b{Int#,Double#} c{Int#,Double#} #-}
-- {-# GENERATE_SPECS amap a{~,Int#,Int,IPr} b{Int#,Double#} c{Int#,Double#} #-}
-- {-# GENERATE_SPECS ixmap a{~,Int#,Int} b{~,Int#,Int} c{Int#,Double#} #-}

-- {-# GENERATE_SPECS instance a{Int#} b{Int#,Double#} :: Eq (Array a b) #-}
-- {-# GENERATE_SPECS instance a{Int#} b{Int#,Double#} :: Ord (Array a b) #-}
-- {-# GENERATE_SPECS instance a{Int#} b{Int#,Double#} :: Text (Array a b) #-}

-- {-# GENERATE_SPECS instance a{Int#} b{Int#,Double#} :: Eq (Assoc a b) #-}
-- {-# GENERATE_SPECS instance a{Int#} b{Int#,Double#} :: Ord (Assoc a b) #-}
-- {-# GENERATE_SPECS instance a{Int#} b{Int#,Double#} :: Ix (Assoc a b) #-}
-- {-# GENERATE_SPECS instance a{Int#} b{Int#,Double#} :: Text (Assoc a b) #-}

#endif

-}
