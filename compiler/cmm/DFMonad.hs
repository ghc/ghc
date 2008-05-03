
module DFMonad
    ( DataflowLattice(..)
    , DataflowAnalysis
    , markFactsUnchanged, factsStatus, getFact, setFact, getExitFact, setExitFact
                        , forgetFact, botFact, setAllFacts, getAllFacts, factsEnv, checkFactMatch
    , addLastOutFact, bareLastOutFacts, forgetLastOutFacts
    , subAnalysis

    , DFA, runDFA
    , DFM, runDFM, liftAnal
    , markGraphRewritten, graphWasRewritten
    , freshBlockId
    , liftUSM
    , module OptimizationFuel
    )
where

import CmmTx
import PprCmm()
import OptimizationFuel
import ZipCfg

import Maybes
import Outputable
import UniqFM
import UniqSupply

import Control.Monad

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


-- There are two monads here:
--   1. DFA, the monad of analysis, which never changes anything
--   2. DFM, the monad of combined analysis and transformation,
--      which needs a UniqSupply and may consume transactions

data DFAState f = DFAState { df_facts :: BlockEnv f
                           , df_exit_fact :: f
                           , df_last_outs :: [(BlockId, f)]
                           , df_facts_change :: ChangeFlag
                           }


data DFState f = DFState { df_uniqs :: UniqSupply
                         , df_rewritten :: ChangeFlag
                         , df_astate :: DFAState f
                         , df_fstate :: FuelState
                         }

newtype DFA fact a = DFA  (DataflowLattice fact -> DFAState fact -> (a, DFAState fact))
newtype DFM fact a = DFM  (DataflowLattice fact -> DFState  fact -> (a, DFState  fact))


liftAnal :: DFA f a -> DFM f a
liftAnal (DFA f) = DFM f'
    where f' l s = let (a, anal) = f l (df_astate s)
                   in  (a, s {df_astate = anal})

initDFAState :: f -> DFAState f
initDFAState bot = DFAState emptyBlockEnv bot [] NoChange

runDFA :: DataflowLattice f -> DFA f a -> a
runDFA lattice (DFA f) = fst $ f lattice (initDFAState $ fact_bot lattice)

runDFM :: UniqSupply -> DataflowLattice f -> DFM f a -> FuelMonad a
runDFM uniqs lattice (DFM f) = FuelMonad (\s -> 
    let (a, s') = f lattice $ DFState uniqs NoChange dfa_state s
    in  (a, df_fstate s'))
  where dfa_state = initDFAState (fact_bot lattice)

class DataflowAnalysis m where
  markFactsUnchanged :: m f ()   -- ^ Useful for starting a new iteration
  factsStatus :: m f ChangeFlag
  subAnalysis :: m f a -> m f a  -- ^ Do a new analysis and then throw away
                                 -- *all* the related state.  Even the Uniques
                                 -- will be reused.

  getFact :: BlockId -> m f f
  setFact :: Outputable f => BlockId -> f -> m f ()
  getExitFact :: m f f
  setExitFact :: Outputable f => f -> m f  ()
  checkFactMatch :: Outputable f =>
                    BlockId -> f -> m f () -- ^ assert fact already at this val
  botFact :: m f f
  forgetFact :: BlockId -> m f ()
  -- | It might be surprising these next two are needed in a pure analysis,
  -- but for some problems we do a 'shallow' rewriting in which a rewritten
  -- graph is not itself considered for further rewriting but merely undergoes 
  -- an analysis.  In this case the results of a forward analysis might produce
  -- new facts that go on BlockId's that reside outside the graph being analyzed.
  -- Thus these 'lastOutFacts' need to be available even in a pure analysis. 
  addLastOutFact :: (BlockId, f) -> m f ()
  bareLastOutFacts :: m f [(BlockId, f)]
  forgetLastOutFacts :: m f ()
  getAllFacts :: m f (BlockEnv f)
  setAllFacts :: BlockEnv f -> m f ()
  factsEnv :: Monad (m f) => m f (BlockId -> f)

  lattice :: m f (DataflowLattice f)
  factsEnv = do { map <- getAllFacts
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
  getExitFact = DFA get
    where get _ s = (df_exit_fact s, s)
  setExitFact a =
    do old <- getExitFact
       DataflowLattice { fact_add_to = add_fact
                       , fact_name = name, fact_do_logging = log } <- lattice
       case add_fact a old of
         TxRes NoChange _ -> return ()
         TxRes SomeChange join -> DFA $ \_ s ->
             let debug = if log then pprTrace else \_ _ a -> a
             in  debug name (pprSetFact "exit" old a join) $
                 ((), s { df_exit_fact = join, df_facts_change = SomeChange })
  getAllFacts = DFA f
    where f _ s = (df_facts s, s)
  setAllFacts env = DFA f
    where f _ s = ((), s { df_facts = env})
  botFact = DFA f
    where f lattice s = (fact_bot lattice, s)
  forgetFact id = DFA f 
    where f _ s = ((), s { df_facts = delFromUFM (df_facts s) id })
  addLastOutFact pair = DFA f
    where f _ s = ((), s { df_last_outs = pair : df_last_outs s })
  bareLastOutFacts = DFA f
    where f _ s = (df_last_outs s, s)
  forgetLastOutFacts = DFA f
    where f _ s = ((), s { df_last_outs = [] })
  checkFactMatch id a =
      do { fact <- lattice
         ; old_a <- getFact id
         ; case fact_add_to fact a old_a of
             TxRes NoChange _ -> return ()
             TxRes SomeChange new ->
               do { facts <- getAllFacts
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
  getExitFact         = liftAnal $ getExitFact 
  setExitFact new     = liftAnal $ setExitFact new
  botFact             = liftAnal $ botFact
  forgetFact id       = liftAnal $ forgetFact id
  addLastOutFact p    = liftAnal $ addLastOutFact p
  bareLastOutFacts    = liftAnal $ bareLastOutFacts
  forgetLastOutFacts  = liftAnal $ forgetLastOutFacts
  getAllFacts         = liftAnal $ getAllFacts
  setAllFacts env     = liftAnal $ setAllFacts env
  checkFactMatch id a = liftAnal $ checkFactMatch id a

  lattice             = liftAnal $ lattice

dfmSubAnalysis :: DFM f a -> DFM f a
dfmSubAnalysis (DFM f) = DFM f'
    where f' l s = let s' = s { df_astate = subAnalysisState (df_astate s) }
                       (a, _) = f l s'
                   in  (a, s)


markGraphRewritten :: DFM f ()
markGraphRewritten = DFM f
    where f _ s = ((), s {df_rewritten = SomeChange})

graphWasRewritten :: DFM f ChangeFlag
graphWasRewritten = DFM f
    where f _ s = (df_rewritten s, s)
                    
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

instance FuelUsingMonad (DFM f) where
  fuelRemaining = extract fuelRemainingInState
  lastFuelPass  = extract lastFuelPassInState
  fuelExhausted = extract fuelExhaustedInState
  fuelDecrement p f f' = DFM (\_ s -> ((), s { df_fstate = fs' s }))
    where fs' s = fuelDecrementState p f f' $ df_fstate s

extract :: (FuelState -> a) -> DFM f a
extract f = DFM (\_ s -> (f $ df_fstate s, s))


pprSetFact :: (Show a, Outputable f) => a -> f -> f -> f -> SDoc
pprSetFact id old a join =
    f4sep [text "at" <+> text (show id),
           text "added" <+> ppr a, text "to" <+> ppr old,
           text "yielding" <+> ppr join]

f4sep :: [SDoc] -> SDoc
f4sep [] = fsep []
f4sep (d:ds) = fsep (d : map (nest 4) ds)
