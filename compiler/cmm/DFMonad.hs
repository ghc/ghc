
module DFMonad
    ( OptimizationFuel
    , DFTx, runDFTx, lastTxPass, txDecrement, txRemaining, txExhausted
    , functionalDFTx

    , DataflowLattice(..)
    , DataflowAnalysis
    , markFactsUnchanged, factsStatus, getFact, setFact, botFact
                        , forgetFact, allFacts, factsEnv, checkFactMatch
    , addLastOutFact, lastOutFacts, forgetLastOutFacts
    , subAnalysis

    , DFA, runDFA
    , DFM, runDFM, liftTx, liftAnal
    , markGraphRewritten
    , freshBlockId
    , liftUSM
    )
where

import CmmTx
import Control.Monad
import Maybes
import PprCmm()
import UniqFM
import UniqSupply
import ZipCfg
import qualified ZipCfg as G

import Outputable

{-

A dataflow monad maintains a mapping from BlockIds to dataflow facts,
where a dataflow fact is a value of type [[a]].  Values of type [[a]]
must form a lattice, as described by type [[Fact a]].

The dataflow engine uses the lattice structure to compute a least
solution to a set of dataflow equations.  To compute a greatest
solution, flip the lattice over.

The engine works by starting at the bottom and iterating to a fixed
point, so in principle we require the bottom element, a join (least
upper bound) operation, and a comparison to find out if a value has
changed (grown).  In practice, the comparison is only ever used in
conjunction with the join, so we have [[fact_add_to]]:

  fact_add_to new old =
     let j = join new old in
     if j <= old then noTx old -- nothing changed
     else aTx j                -- the fact changed

-}

data DataflowLattice a = DataflowLattice  { 
  fact_name    :: String,                 -- documentation
  fact_bot     :: a,                      -- lattice bottom element
  fact_add_to  :: a -> a -> TxRes a,      -- lattice join and compare
    -- ^ compute join of two args; something changed iff join is greater than 2nd arg
  fact_do_logging :: Bool  -- log changes
}


-- There are three monads here:
--   1. DFTx, the monad of transactions, to be carried through all
--      graph-changing computations in the program
--   2. DFA, the monad of analysis, which never changes anything
--   3. DFM, the monad of combined analysis and transformation,
--      which needs a UniqSupply and may consume transactions

data DFAState f = DFAState { df_facts :: BlockEnv f
                           , df_facts_change :: ChangeFlag
                           }

data DFTxState = DFTxState { df_txlimit :: OptimizationFuel, df_lastpass :: String }

data DFState f = DFState { df_uniqs :: UniqSupply
                         , df_rewritten :: ChangeFlag
                         , df_astate :: DFAState f
                         , df_txstate :: DFTxState
                         , df_last_outs :: [(BlockId, f)]
                         }

newtype DFTx a     = DFTx (DFTxState     -> (a, DFTxState))
newtype DFA fact a = DFA  (DataflowLattice fact -> DFAState fact -> (a, DFAState fact))
newtype DFM fact a = DFM  (DataflowLattice fact -> DFState  fact -> (a, DFState  fact))


liftAnal :: DFA f a -> DFM f a
liftAnal (DFA f) = DFM f'
    where f' l s = let (a, anal) = f l (df_astate s)
                   in  (a, s {df_astate = anal})

liftTx :: DFTx a -> DFM f a
liftTx (DFTx f) = DFM f'
    where f' _ s = let (a, txs) = f (df_txstate s)
                   in  (a, s {df_txstate = txs})

newtype OptimizationFuel = OptimizationFuel Int
  deriving (Ord, Eq, Num, Show, Bounded)

initDFAState :: DFAState f
initDFAState = DFAState emptyBlockEnv NoChange

runDFA :: DataflowLattice f -> DFA f a -> a
runDFA lattice (DFA f) = fst $ f lattice initDFAState

-- XXX DFTx really needs to be in IO, so we can dump programs in
-- intermediate states of optimization ---NR

functionalDFTx :: String -> (OptimizationFuel -> (a, OptimizationFuel)) -> DFTx a
functionalDFTx name pass = DFTx f
    where f s = let (a, fuel) = pass (df_txlimit s)
                in  (a, DFTxState fuel name)

runDFTx :: OptimizationFuel -> DFTx a -> a  --- should only be called once per program!
runDFTx lim (DFTx f) = fst $ f $ DFTxState lim "<none>"

lastTxPass :: DFTx String
lastTxPass = DFTx f
    where f s = (df_lastpass s, s)

runDFM :: UniqSupply -> DataflowLattice f -> DFM f a -> DFTx a
runDFM uniqs lattice (DFM f) = DFTx f'
    where f' txs =
            let (a, s) = f lattice $ DFState uniqs NoChange initDFAState txs [] in
            (a, df_txstate s)

txExhausted :: DFTx Bool
txExhausted = DFTx f
    where f s = (df_txlimit s <= 0, s)

txRemaining :: DFTx OptimizationFuel
txRemaining = DFTx f
    where f s = (df_txlimit s, s)

txDecrement :: String -> OptimizationFuel -> OptimizationFuel -> DFTx ()
txDecrement optimizer old new = DFTx f
    where f s = ((), s { df_txlimit = lim s, df_lastpass = optimizer })
          lim s = if old == df_txlimit s then new
                  else panic $ concat ["lost track of ", optimizer, "'s transactions"]


class DataflowAnalysis m where
  markFactsUnchanged :: m f ()   -- ^ Useful for starting a new iteration
  factsStatus :: m f ChangeFlag
  subAnalysis :: m f a -> m f a  -- ^ Do a new analysis and then throw away
                                 -- *all* the related state.  Even the Uniques
                                 -- will be reused.

  getFact :: BlockId -> m f f
  setFact :: Outputable f => BlockId -> f -> m f ()
  checkFactMatch :: Outputable f =>
                    BlockId -> f -> m f () -- ^ assert fact already at this val
  botFact :: m f f
  forgetFact :: BlockId -> m f ()
  forgetLastOutFacts :: m f ()
  allFacts :: m f (BlockEnv f)
  factsEnv :: Monad (m f) => m f (BlockId -> f)

  lattice :: m f (DataflowLattice f)
  factsEnv = do { map <- allFacts
                ; bot <- botFact
                ; return $ \id -> lookupBlockEnv map id `orElse` bot }

instance DataflowAnalysis DFA where
  markFactsUnchanged = DFA f
    where f _ s = ((), s {df_facts_change = NoChange}) 
  factsStatus = DFA f'
    where f' _ s = (df_facts_change s, s)
  subAnalysis (DFA f) = DFA f'
    where f' l s = let (a, _) = f l (subAnalysisState s) in (a, s)
  getFact id = DFA get
    where get lattice s = (lookupBlockEnv (df_facts s) id `orElse` fact_bot lattice, s)
  setFact id a =
    do old <- getFact id
       DataflowLattice { fact_add_to = add_fact
                       , fact_name = name, fact_do_logging = log } <- lattice
       case add_fact a old of
         TxRes NoChange _ -> return ()
         TxRes SomeChange join -> DFA $ \_ s ->
             let facts' = extendBlockEnv (df_facts s) id join
                 debug = if log then pprTrace else \_ _ a -> a
             in  debug name (pprSetFact id old a join) $
                 ((), s { df_facts = facts', df_facts_change = SomeChange })
  botFact = DFA f
    where f lattice s = (fact_bot lattice, s)
  forgetFact id = DFA f 
    where f _ s = ((), s { df_facts = delFromUFM (df_facts s) id })
  forgetLastOutFacts = return ()
  allFacts = DFA f
    where f _ s = (df_facts s, s)
  checkFactMatch id a =
      do { fact <- lattice
         ; old_a <- getFact id
         ; case fact_add_to fact a old_a of
             TxRes NoChange _ -> return ()
             TxRes SomeChange new ->
               do { facts <- allFacts
                  ; pprPanic "checkFactMatch"
                            (f4sep [text (fact_name fact), text "at id" <+> ppr id,
                                    text "changed from", nest 4 (ppr old_a), text "to",
                                    nest 4 (ppr new),
                                    text "after supposedly reaching fixed point;",
                                    text "env is", pprFacts facts]) 
                  ; setFact id a }
         }
    where pprFacts env = vcat (map pprFact (ufmToList env))
          pprFact (id, a) = hang (ppr id <> colon) 4 (ppr a)

  lattice = DFA f
    where f l s = (l, s)

subAnalysisState :: DFAState f -> DFAState f
subAnalysisState s = s {df_facts_change = NoChange}


instance DataflowAnalysis DFM where
  markFactsUnchanged  = liftAnal $ markFactsUnchanged
  factsStatus         = liftAnal $ factsStatus
  subAnalysis         = dfmSubAnalysis
  getFact id          = liftAnal $ getFact id
  setFact id new      = liftAnal $ setFact id new
  botFact             = liftAnal $ botFact
  forgetFact id       = liftAnal $ forgetFact id
  forgetLastOutFacts  = dfmForgetLastOutFacts
  allFacts            = liftAnal $ allFacts
  checkFactMatch id a = liftAnal $ checkFactMatch id a

  lattice             = liftAnal $ lattice

dfmSubAnalysis :: DFM f a -> DFM f a
dfmSubAnalysis (DFM f) = DFM f'
    where f' l s = let s' = s { df_astate = subAnalysisState (df_astate s) }
                       (a, _) = f l s'
                   in  (a, s)

dfmForgetLastOutFacts :: DFM f ()
dfmForgetLastOutFacts = DFM f
    where f _ s = ((), s { df_last_outs = [] })

addLastOutFact :: (BlockId, f) -> DFM f ()
addLastOutFact pair = DFM f
    where f _ s = ((), s { df_last_outs = pair : df_last_outs s })

lastOutFacts :: DFM f [(BlockId, f)]
lastOutFacts = DFM f
    where f _ s = (df_last_outs s, s)

markGraphRewritten :: DFM f ()
markGraphRewritten = DFM f
    where f _ s = ((), s {df_rewritten = SomeChange})

freshBlockId :: String -> DFM f BlockId
freshBlockId _s = liftUSM $ getUniqueUs >>= return . BlockId

liftUSM :: UniqSM a -> DFM f a
liftUSM uc = DFM f
    where f _ s = let (a, us') = initUs (df_uniqs s) uc
                  in (a, s {df_uniqs = us'})

instance Monad (DFA f) where
  DFA f >>= k = DFA (\l s -> let (a, s') = f l s
                                 DFA f' = k a
                             in  f' l s')
  return a = DFA (\_ s -> (a, s))

instance Monad (DFM f) where
  DFM f >>= k = DFM (\l s -> let (a, s') = f l s
                                 DFM f' = k a
                             in  f' l s')
  return a = DFM (\_ s -> (a, s))

instance Monad (DFTx) where
  DFTx f >>= k = DFTx (\s -> let (a, s') = f s
                                 DFTx f' = k a
                             in  f' s')
  return a = DFTx (\s -> (a, s))

pprSetFact :: Outputable f => BlockId -> f -> f -> f -> SDoc
pprSetFact id old a join =
    f4sep [text "at" <+> text (show id),
           text "added" <+> ppr a, text "to" <+> ppr old,
           text "yielding" <+> ppr join]

f4sep :: [SDoc] -> SDoc
f4sep [] = fsep []
f4sep (d:ds) = fsep (d : map (nest 4) ds)


_I_am_abstract :: Int -> OptimizationFuel
_I_am_abstract = OptimizationFuel -- prevents warning: OptimizationFuel unused
