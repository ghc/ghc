--!!! Testing (one aspect of) the dictionary bug
{-
Hello,

Thanks for your reply and advice about the GC debugging. Before I got
it, (our mail server is slow and undeterministic for incoming mail,
and I have to call it up manually) I had boiled down my program to a
quite simple test example, and prepared a mail to send to you.

I don't know if the two problems are related. With my test program,
the bug occurs only after a (manual) GC. Each time. I have to
reload the script to get it going again.

The following is the mail I intended to send, with enclosed test
program:

Hi Alastair,

I have verified that there is a garbage collection related bug in
Hugs 1.01, both in the unpatched and the patched version, compiled
for Linux. The unpatched one had no changes to the source expect
SUNOS 0 and LINUX 1 in prelude.h

I have boiled it down to a simple test program.  The program won't
compile in either Gofer or Hugs 1.0!  This seems suspicious to me,
but maybe the program can be simplified further.

I still suspect it has something to do with the dictionaries not
being marked correctly.

Maybe this will be of some relevance for your new GC as well.

I don't know what / if there is a Hugs bug mailing list, maybe
you will forward this there or to Mark directly?

I'll tell you if I find out anything more specific.

It seems pretty certain the problem has nothing to do with that the
suspicious thing begins on Line 13, though...

Sverker

PS: Boiled down bug-provoking program enclosed, tbug.gs:

-}
module TestDicts where

class T a where			-- Line 1
	t :: Int ->  a

instance T Int where
	t = id

instance (T a, T b) => T (a, b) where
	t p = 
	    (t p, t p)


instance (T a, T b, T c) => T (a, b, c) where
	t p = 				-- Line 13
	    (a, b, c) where
			(a, (b, c)) = t p
-- The following seems to give the same effect:
--	t p = 
--	   case t (p + 3) of
--		(a, (b, c)) -> (a, b, c)
-- But the following seems to work:
--	t p = (t p, t p, t p)


t2:: Int -> (Int,Int)
t2 = t			-- t2 has no problems

t3:: Int -> (Int,Int,Int)
t3 = t			-- t3 has problems, see session transcript


{-

-- Gofer or Hugs 1.0 would not allow this program. Extract from Hugs 1.0:

? :l /home/nilsson/ngof/simpleprims/src/tbug.gs
Reading script file "/home/nilsson/ngof/simpleprims/src/tbug.gs":
Type checking      
ERROR "/home/nilsson/ngof/simpleprims/src/tbug.gs" (line 13): Insufficient class constraints in instance member binding
*** Context  : (T a, T b, T c)
*** Required : T d

-- Hugs 1.01 allows it, as well as hacked.hugs. But in both the GC bug occurs.
-- Extract from Hugs 1.01:

Hugs session for:
/usr/local/lib/Hugs/hugs.prelude
tbug.gs
? t3 14
(14,14,14)
? :gc
Garbage collection recovered 94995 cells
? t3 14
(

INTERNAL ERROR: Error in graph
? t3 17
(
INTERNAL ERROR: Error in graph
? 

-- Rewriting the tbug.gs file and reloading restores conditions.

Hugs session for:
/usr/local/lib/Hugs/hugs.prelude
tbug.gs
? t3 14
(14,14,14)
? :gc
Garbage collection recovered 94995 cells
? t3 14
(
INTERNAL ERROR: Error in graph

-}

