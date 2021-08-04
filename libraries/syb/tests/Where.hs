{-# OPTIONS -fglasgow-exts #-}

module Where (tests) where

{-

This example illustrates some differences between certain traversal
schemes. To this end, we use a simple system of datatypes, and the
running example shall be to replace "T1a 42" by "T1a 88". It is our
intention to illustrate a few dimensions of designing traversals.

1. We can decide on whether we prefer "rewrite steps" (i.e.,
monomorphic functions on data) that succeed either for all input
patterns or only if the encounter a term pattern to be replaced. In
the first case, the catch-all equation of such a function describes
identity (see "stepid" below). In the second case, the catch-call
equation describes failure using the Maybe type constructor (see
"stepfail" below). As an intermediate assessment, the failure approach
is more general because it allows one to observe if a rewrite step was
meaningful or not. Often the identity approach is more convenient and
sufficient.

2. We can now also decide on whether we want monadic or simple
traversals; recall monadic generic functions GenericM from
Data.Generics.  The monad can serve for success/failure, state,
environment and others.  One can now subdivide monadic traversal
schemes with respect to the question whether they simply support
monadic style of whether they even interact with the relevant
monad. The scheme "everywereM" from the library belongs to the first
category while "somewhere" belongs to the second category as it uses
the operation "mplus" of a monad with addition. So while "everywhereM"
makes very well sense without a monad --- as demonstrated by
"everywhere", the scheme "somewhere" is immediately monadic.

3. We can now also decide on whether we want rewrite steps to succeed
for all possible subterms, at least for one subterm, exactly for one
subterm, and others.  The various traversal schemes make different
assumptions in this respect.

a) everywhere

   By its type, succeeds and requires non-failing rewrite steps.
   However, we do not get any feedback on whether terms were actually
   rewritten. (Say, we might have performed accidentally the identity
   function on all nodes.)

b) everywhereM

   Attempts to reach all nodes where all the sub-traversals are performed
   in monadic bind-sequence. Failure of the traversal for a given subterm
   implies failure of the entire traversal. Hence, the argument of
   "everywhereM" should be designed in a way that it tends to succeed
   except for the purpose of propagating a proper error in the sense of
   violating a pre-/post-condition. For example, "mkM stepfail" should
   not be passed to "everywhereM" as it will fail for all but one term
   pattern; see "recovered" for a way to massage "stepfail" accordingly.

c) somewhere

   Descends into term in a top-down manner, and stops in a given
   branch when the argument succeeds for the subterm at hand. To this
   end, it takes an argument that is perfectly intended to fail for
   certain term patterns. Thanks to the employment of gmapF, the
   traversal scheme recovers from failure when mapping over the immediate
   subterms while insisting success for at least one subterm (say, branch).
   This scheme is appropriate if you want to make sure that a given
   rewrite step was actually used in a traversal. So failure of the
   traversal would mean that the argument failed for all subterms.

Contributed by Ralf Laemmel, ralf@cwi.nl

-}

import Test.Tasty.HUnit

import Data.Generics
import Control.Monad


-- Two mutually recursive datatypes
data T1 = T1a Int | T1b T2  deriving (Typeable, Data)
data T2 = T2 T1             deriving (Typeable, Data)


-- A rewrite step with identity as catch-all case
stepid (T1a 42) = T1a 88
stepid x        = x


-- The same rewrite step but now with failure as catch-all case
stepfail (T1a 42) = Just (T1a 88)
stepfail _        = Nothing


-- We can let recover potentially failing generic functions from failure;
-- this is illustrated for a generic made from stepfail via mkM.
recovered x = mkM stepfail x `mplus` Just x


-- A test term that comprehends a redex
term42 = T1b (T2 (T1a 42))


-- A test term that does not comprehend a redex
term37 = T1b (T2 (T1a 37))


-- A number of traversals
result1 = everywhere (mkT stepid)    term42   -- rewrites term accordingly
result2 = everywhere (mkT stepid)    term37   -- preserves term without notice
result3 = everywhereM (mkM stepfail) term42   -- fails in a harsh manner
result4 = everywhereM (mkM stepfail) term37   -- fails rather early
result5 = everywhereM recovered      term37   -- preserves term without notice
result6 = somewhere (mkMp stepfail)  term42   -- rewrites term accordingly
result7 = somewhere (mkMp stepfail)  term37   -- fails to notice lack of redex

tests = gshow ( result1,
              ( result2,
              ( result3,
              ( result4,
              ( result5,
              ( result6,
              ( result7 ))))))) @=? output

output = "((,) (T1b (T2 (T1a (88)))) ((,) (T1b (T2 (T1a (37)))) ((,) (Nothing) ((,) (Nothing) ((,) (Just (T1b (T2 (T1a (37))))) ((,) (Just (T1b (T2 (T1a (88))))) (Nothing)))))))"
