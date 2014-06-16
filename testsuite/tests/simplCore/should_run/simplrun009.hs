{-# LANGUAGE ExistentialQuantification #-}

-- This test is really meant for human looking; do a -ddump-simpl.

-- The definition that you want to look at is for foo.
-- It produces a nested unfold that should look something
-- like the code below.  Note the 'lvl1_shW'.  It is BAD
-- if this is a lambda instead; you get a lot more allocation
-- See Note [Escaping a value lambda] in SetLevels


{-
      $wunfold_shU =
	\ (ww_she :: [[a_abm]]) (ww1_shf :: Data.Maybe.Maybe (Stream.Stream a_abm)) ->
	  case ww1_shf of wild2_afo {
	    Data.Maybe.Nothing ->
	      case ww_she of wild_ad6 {
		[] -> GHC.Base.[] @ a_abm;
		: x_ado xs1_adp ->
		  $wunfold_shU
		    xs1_adp
		    (Data.Maybe.Just
		       @ (Stream.Stream a_abm) (Stream.Stream @ a_abm @ [a_abm] 
								*** lvl1_shW *** 
								x_ado))
	      };
	    Data.Maybe.Just ds3_afJ ->
	      case ds3_afJ of wild3_afL { Stream.Stream @ s1_afN stepb_afO sb_afP ->
	      case stepb_afO sb_afP of wild4_afR {
		Stream.Done -> $wunfold_shU ww_she (Data.Maybe.Nothing @ (Stream.Stream a_abm));
		Stream.Yield x_afV sb'_afW ->
		  GHC.Base.:
		    @ a_abm
		    x_afV
		    ($wunfold_shU
		       ww_she
		       (Data.Maybe.Just
			  @ (Stream.Stream a_abm) (Stream.Stream @ a_abm @ s1_afN stepb_afO sb'_afW)));
		Stream.Skip sb'_afZ ->
		  $wunfold_shU
		    ww_she
		    (Data.Maybe.Just
		       @ (Stream.Stream a_abm) (Stream.Stream @ a_abm @ s1_afN stepb_afO sb'_afZ))
	      }
	      }
-}



module Main( main, foo ) where
-- Must export foo to make the issue show up

import Prelude hiding ( concatMap, map) 

main = print (sum (foo [[1,2], [3,4,5]]))

foo :: Num a => [[a]] -> [a]
foo xss = Main.concatMap (\xs -> Main.map (+1) xs) xss


instance StreamableSequence [] where
  stream = listToStream
  unstream = streamToList
  -- These inline pragmas are useless (see #5084)
{-
  {-# INLINE stream #-}
  {-# INLINE unstream #-}
-}

listToStream :: [a] -> Stream a
listToStream xs = Stream next xs
  where next []     = Done
        next (x:xs) = Yield x xs
{-# INLINE [0] listToStream #-}

streamToList :: Stream a -> [a]
streamToList (Stream next s) = unfold s
  where unfold s =
          case next s of
            Done       -> []
            Skip    s' ->     unfold s'
            Yield x s' -> x : unfold s'
{-# INLINE [0] streamToList #-}

{-# RULES
"stream/unstream"
  forall s. listToStream (streamToList s) = s
  #-}
            
map :: (a -> b) -> [a] -> [b]
map f = unstream . mapS f . stream
{-# INLINE map #-}

concatMap :: (a -> [b]) -> [a] -> [b]
concatMap f = unstream . concatMapS (stream . f) . stream
{-# INLINE concatMap #-}


data Stream a = forall s. Stream (s -> Step a s) s

data Step a s = Done
              | Yield a s
              | Skip    s

class StreamableSequence seq where
  stream   :: seq a -> Stream a
  unstream :: Stream a -> seq a

  -- axiom: stream . unstream = id
  -- These inline pragmas are useless (see #5084)
{-
  {-# INLINE stream #-}
  {-# INLINE unstream #-}
-}

{-
--version that does not require the sequence type
--to be polymorphic in its elements:

class StreamableSequence seq a | seq -> a where
  stream   :: seq -> Stream a
  unstream :: Stream a -> seq
-}


mapS :: (a -> b) -> Stream a -> Stream b
mapS f (Stream next s0) = Stream next' s0
  where next' s = case next s of
          Done       -> Done
          Skip    s' -> Skip        s'
          Yield x s' -> Yield (f x) s'
{-# INLINE [0] mapS #-}

            
concatMapS :: (a -> Stream b) -> Stream a -> Stream b
concatMapS f (Stream step s) = Stream step' (s, Nothing)
  where step' (s, Nothing) =
          case step s of
            Yield x s' -> Skip (s', Just (f x))
            Skip    s' -> Skip (s', Nothing)
            Done       -> Done

        step' (s, Just (Stream stepb sb)) =
          case stepb sb of
            Yield x sb' -> Yield x (s, Just (Stream stepb sb'))
            Skip    sb' -> Skip (s, Just (Stream stepb sb'))
            Done        -> Skip (s, Nothing)
{-# INLINE [0] concatMapS #-}

