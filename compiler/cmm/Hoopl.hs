module Hoopl (
    module Compiler.Hoopl,
    module Hoopl.Dataflow,
    deepFwdRw, deepFwdRw3,
    deepBwdRw, deepBwdRw3,
    thenFwdRw
  ) where

import Compiler.Hoopl hiding
  ( (<*>), mkLabel, mkBranch, mkMiddle, mkLast, -- clashes with our MkGraph
    Unique,
    FwdTransfer(..), FwdRewrite(..), FwdPass(..),
    BwdTransfer(..), BwdRewrite(..), BwdPass(..),
    noFwdRewrite, noBwdRewrite,
    analyzeAndRewriteFwd, analyzeAndRewriteBwd,
    mkFactBase, Fact,
    mkBRewrite, mkBRewrite3, mkBTransfer, mkBTransfer3,
    mkFRewrite, mkFRewrite3, mkFTransfer, mkFTransfer3,
    deepFwdRw, deepFwdRw3, thenFwdRw, iterFwdRw,
    deepBwdRw, deepBwdRw3, thenBwdRw, iterBwdRw
  )

import Hoopl.Dataflow
import Control.Monad
import UniqSupply

deepFwdRw3 :: (n C O -> f -> UniqSM (Maybe (Graph n C O)))
           -> (n O O -> f -> UniqSM (Maybe (Graph n O O)))
           -> (n O C -> f -> UniqSM (Maybe (Graph n O C)))
           -> (FwdRewrite UniqSM n f)
deepFwdRw :: (forall e x . n e x -> f -> UniqSM (Maybe (Graph n e x))) -> FwdRewrite UniqSM n f
deepFwdRw3 f m l = iterFwdRw $ mkFRewrite3 f m l
deepFwdRw f = deepFwdRw3 f f f

-- N.B. rw3, rw3', and rw3a are triples of functions.
-- But rw and rw' are single functions.
thenFwdRw :: forall n f.
             FwdRewrite UniqSM n f
          -> FwdRewrite UniqSM n f
          -> FwdRewrite UniqSM n f
thenFwdRw rw3 rw3' = wrapFR2 thenrw rw3 rw3'
 where
  thenrw :: forall e x t t1.
               (t -> t1 -> UniqSM (Maybe (Graph n e x, FwdRewrite UniqSM n f)))
            -> (t -> t1 -> UniqSM (Maybe (Graph n e x, FwdRewrite UniqSM n f)))
            -> t
            -> t1
            -> UniqSM (Maybe (Graph n e x, FwdRewrite UniqSM n f))
  thenrw rw rw' n f = rw n f >>= fwdRes
     where fwdRes Nothing   = rw' n f
           fwdRes (Just gr) = return $ Just $ fadd_rw rw3' gr

iterFwdRw :: forall n f.
             FwdRewrite UniqSM n f
          -> FwdRewrite UniqSM n f
iterFwdRw rw3 = wrapFR iter rw3
 where iter :: forall a e x t.
               (t -> a -> UniqSM (Maybe (Graph n e x, FwdRewrite UniqSM n f)))
               -> t
               -> a
               -> UniqSM (Maybe (Graph n e x, FwdRewrite UniqSM n f))
       iter rw n = (liftM $ liftM $ fadd_rw (iterFwdRw rw3)) . rw n

-- | Function inspired by 'rew' in the paper
_frewrite_cps :: ((Graph n e x, FwdRewrite UniqSM n f) -> UniqSM a)
             -> UniqSM a
             -> (forall e x . n e x -> f -> UniqSM (Maybe (Graph n e x, FwdRewrite UniqSM n f)))
             -> n e x
             -> f
             -> UniqSM a
_frewrite_cps j n rw node f =
    do mg <- rw node f
       case mg of Nothing -> n
                  Just gr -> j gr



-- | Function inspired by 'add' in the paper
fadd_rw :: FwdRewrite UniqSM n f
        -> (Graph n e x, FwdRewrite UniqSM n f)
        -> (Graph n e x, FwdRewrite UniqSM n f)
fadd_rw rw2 (g, rw1) = (g, rw1 `thenFwdRw` rw2)



deepBwdRw3 ::
              (n C O -> f          -> UniqSM (Maybe (Graph n C O)))
           -> (n O O -> f          -> UniqSM (Maybe (Graph n O O)))
           -> (n O C -> FactBase f -> UniqSM (Maybe (Graph n O C)))
           -> (BwdRewrite UniqSM n f)
deepBwdRw  :: (forall e x . n e x -> Fact x f -> UniqSM (Maybe (Graph n e x)))
           -> BwdRewrite UniqSM n f
deepBwdRw3 f m l = iterBwdRw $ mkBRewrite3 f m l
deepBwdRw  f = deepBwdRw3 f f f


thenBwdRw :: forall n f. BwdRewrite UniqSM n f -> BwdRewrite UniqSM n f -> BwdRewrite UniqSM n f
thenBwdRw rw1 rw2 = wrapBR2 f rw1 rw2
  where f :: forall t t1 t2 e x.
             t
             -> (t1 -> t2 -> UniqSM (Maybe (Graph n e x, BwdRewrite UniqSM n f)))
             -> (t1 -> t2 -> UniqSM (Maybe (Graph n e x, BwdRewrite UniqSM n f)))
             -> t1
             -> t2
             -> UniqSM (Maybe (Graph n e x, BwdRewrite UniqSM n f))
        f _ rw1 rw2' n f = do
          res1 <- rw1 n f
          case res1 of
            Nothing -> rw2' n f
            Just gr -> return $ Just $ badd_rw rw2 gr

iterBwdRw :: forall n f. BwdRewrite UniqSM n f -> BwdRewrite UniqSM n f
iterBwdRw rw = wrapBR f rw
  where f :: forall t e x t1 t2.
             t
             -> (t1 -> t2 -> UniqSM (Maybe (Graph n e x, BwdRewrite UniqSM n f)))
             -> t1
             -> t2
             -> UniqSM (Maybe (Graph n e x, BwdRewrite UniqSM n f))
        f _ rw' n f = liftM (liftM (badd_rw (iterBwdRw rw))) (rw' n f)

-- | Function inspired by 'add' in the paper
badd_rw :: BwdRewrite UniqSM n f
        -> (Graph n e x, BwdRewrite UniqSM n f)
        -> (Graph n e x, BwdRewrite UniqSM n f)
badd_rw rw2 (g, rw1) = (g, rw1 `thenBwdRw` rw2)

-- Note [Deprecations in Hoopl]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- CmmLive and CmmBuildInfoTables modules enable -fno-warn-warnings-deprecations
-- flag because they import deprecated functions from Hoopl. I spent some time
-- trying to figure out what is going on, so here's a brief explanation. The
-- culprit is the joinOutFacts function, which should be replaced with
-- joinFacts. The difference between them is that the latter one needs extra
-- Label parameter. Labels identify blocks and are used in the fact base to
-- assign facts to a block (in case you're wondering, Label is an Int wrapped in
-- a newtype). Lattice join function is also required to accept a Label but the
-- only reason why it is so are the debugging purposes: see joinInFacts function
-- which is a no-op and is run only because join function might produce
-- debugging output. Now, going back to the Cmm modules. The "problem" with the
-- deprecated joinOutFacts function is that it passes wrong label when calling
-- lattice join function: instead of label of a block for which we are joining
-- facts it uses labels of successors of that block. So the joinFacts function
-- expects to be given a label of a block for which we are joining facts. I
-- don't see an obvious way of recovering that Label at the call sites of
-- joinOutFacts (if that was easily done then joinFacts function could do it
-- internally without requiring label as a parameter). A cheap way of
-- eliminating these warnings would be to create a bogus Label, since none of
-- our join functions is actually using the Label parameter. But that doesn't
-- feel right. I think the real solution here is to fix Hoopl API, which is
-- already broken in several ways. See Hoopl/Cleanup page on the wiki for more
-- notes on improving Hoopl.
