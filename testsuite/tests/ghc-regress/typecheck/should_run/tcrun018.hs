{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

class Monad m => C1 m x

class (Monad m, C1 m x) => C2 m x
  where
    c2 :: x -> m x

class C1 m x => C3 m x
  where
    c3 :: x -> m x

instance Monad m => C1 m Bool

instance C2 Maybe Bool
  where
    c2 = return

instance C3 Maybe Bool
  where
    c3 = return


main = do { print (c2 True :: Maybe Bool) ;
	    print (c3 True :: Maybe Bool) }

------------------------------------------------------------------------
{-	Here's the email from Ralf Laemmel
	reporting a bug in Hugs

1. If you evaluate "test",
   then you get as expected "Just True".

2. Now remove the "Monad M" constraint
   in the class C2. [giving the class C3] 
   This is of course legal and semantics-preserving 
   since the monad constraints is implied by C1
   anyway.

3. Now evaluate "test" again. Oops, it diverges.

I did this Hugs Version February 2001 under Solaris
and Linux. Command line option -98 is needed of course.
Funny enough, if CTRL-C the evaluation of "test"
and you try it the second time (without reloading
anything etc.), then you see "Program error: {_Gc Black
Hole}". Of course, there is no such black hole.

I extracted the above fragment from a huge problem.
The scheme is the following. It seems have to do 
with multi-parameter classes. It definitely has to
do with multi-layered class hierarchies where one
class has a class-wide superclass, as C2 has C1 in
the example. It seems that the superclass is
properly propagated during type-inference/checking
but it is not properly propagated, as for as code
determination in overloading resolution.

Please, let me know if I can be of further assistance.
I am actually amazed how general this scheme is
(3 classes, 2 parameters). How does it come that it
was not detected much earlier.

Anyway, good luck,
Ralf


P.S.: I have more problems in a larger application.
Certain overloaded expressions diverge. I can recover
from that usually by using explicit function types
for top-level functions, where again -- as in C1 above
-- I repeat some constraints which are derivable. I was
not able to extract a simple program for that problem.
But I would be glad to help you by checking if the
problem is gone after you did the bug fix.


-- 
Dr.-Ing. Ralf Laemmel
CWI & VU, Amsterdam, The Netherlands
http://www.cwi.nl/~ralf/
http://www.cs.vu.nl/~ralf/

-}