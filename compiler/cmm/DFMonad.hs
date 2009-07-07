module DFMonad
    ( DataflowLattice(..) , DataflowAnalysis
    , markFactsUnchanged, factsStatus, getFact, setFact, getExitFact, setExitFact
                        , forgetFact, botFact, setAllFacts, getAllFacts, factsEnv
    , addLastOutFact, bareLastOutFacts, forgetLastOutFacts, checkFactMatch
    , subAnalysis

    , DFM, runDFM, liftToDFM
    , markGraphRewritten, graphWasRewritten
    , module OptimizationFuel
    )
where

import BlockId
import CmmTx
import PprCmm()
import OptimizationFuel

import Maybes
import Outputable
import UniqSupply

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
  fact_name       :: String,                 -- documentation
  fact_bot        :: a,                      -- lattice bottom element
  fact_add_to     :: a -> a -> TxRes a,      -- lattice join and compare
    -- ^ compute join of two args; something changed iff join is greater than 2nd arg
  fact_do_logging :: Bool                    -- log changes
}


-- DFM is the monad of combined analysis and transformation,
-- which needs a UniqSupply and may consume optimization fuel
-- DFM is defined using a monad transformer, DFM', which is the general
-- case of DFM, parameterized over any monad.
-- In practice, we apply DFM' to the FuelMonad, which provides optimization fuel and
-- the unique supply.
data DFState f = DFState { df_rewritten    :: !ChangeFlag
                         , df_facts        :: !(BlockEnv f)
                         , df_exit_fact    :: !f
                         , df_last_outs    :: ![(BlockId, f)]
                         , df_facts_change :: !ChangeFlag
                         }

newtype DFM' m fact a = DFM' (DataflowLattice fact -> DFState fact
                                                   -> m (a, DFState  fact))
type DFM fact a = DFM' FuelMonad fact a


runDFM :: Monad m => DataflowLattice f -> DFM' m f a -> m a
runDFM lattice (DFM' f) =
  (f lattice $ DFState NoChange emptyBlockEnv (fact_bot lattice) [] NoChange)
  >>= return . fst

class DataflowAnalysis m where
  markFactsUnchanged :: m f ()   -- ^ Useful for starting a new iteration
  factsStatus :: m f ChangeFlag
  subAnalysis :: m f a -> m f a  -- ^ Do a new analysis and then throw away
                                 -- /all/ the related state.

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

instance Monad m => DataflowAnalysis (DFM' m) where
  markFactsUnchanged = DFM' f
    where f _ s = return ((), s {df_facts_change = NoChange}) 
  factsStatus = DFM' f'
    where f' _ s = return (df_facts_change s, s)
  subAnalysis (DFM' f) = DFM' f'
    where f' l s = do (a, _) <- f l (subAnalysisState s)
                      return (a, s)
  getFact id = DFM' get
    where get lattice s =
            return (lookupBlockEnv (df_facts s) id `orElse` fact_bot lattice, s)
  setFact id a = DFM' set
    where set (DataflowLattice name bot add_fact log) s =
            case add_fact a old of
                 TxRes NoChange _ -> if initialized then return ((), s) else update old old
                 TxRes SomeChange join -> update join old
              where (old, initialized) =
                      case lookupBlockEnv (df_facts s) id of
                        Just f  -> (f,   True)
                        Nothing -> (bot, False)
                    update join old =
                      let facts' = extendBlockEnv (df_facts s) id join
                          debug = if log then pprTrace else \_ _ a -> a
                      in  debug name (pprSetFact id old a join) $
                          return ((), s { df_facts = facts', df_facts_change = SomeChange })
  getExitFact = DFM' get
    where get _ s = return (df_exit_fact s, s)
  setExitFact a =
    do DataflowLattice { fact_name = name, fact_do_logging = log} <- lattice
       DFM' $ \_ s ->
                let debug = if log then pprTrace else \_ _ a -> a
                in  debug name (pprSetFact "exit" a a a) $
                    return ((), s { df_exit_fact = a })
  getAllFacts = DFM' f
    where f _ s = return (df_facts s, s)
  setAllFacts env = DFM' f
    where f _ s = return ((), s { df_facts = env})
  botFact = DFM' f
    where f lattice s = return (fact_bot lattice, s)
  forgetFact id = DFM' f 
    where f _ s = return ((), s { df_facts = delFromBlockEnv (df_facts s) id })
  addLastOutFact pair = DFM' f
    where f _ s = return ((), s { df_last_outs = pair : df_last_outs s })
  bareLastOutFacts = DFM' f
    where f _ s = return (df_last_outs s, s)
  forgetLastOutFacts = DFM' f
    where f _ s = return ((), s { df_last_outs = [] })
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
                                    text "env is", pprFacts facts]) }
         }
    where pprFacts env = vcat (map pprFact (blockEnvToList env))
          pprFact (id, a) = hang (ppr id <> colon) 4 (ppr a)

  lattice = DFM' f
    where f l s = return (l, s)

subAnalysisState :: DFState f -> DFState f
subAnalysisState s = s {df_facts_change = NoChange}


markGraphRewritten :: Monad m => DFM' m f ()
markGraphRewritten = DFM' f
    where f _ s = return ((), s {df_rewritten = SomeChange})

graphWasRewritten :: DFM f ChangeFlag
graphWasRewritten = DFM' f
    where f _ s = return (df_rewritten s, s)
                    
instance Monad m => Monad (DFM' m f) where
  DFM' f >>= k = DFM' (\l s -> do (a, s') <- f l s
                                  s' `seq` case k a of DFM' f' -> f' l s')
  return a = DFM' (\_ s -> return (a, s))
 -- The `seq` is essential to ensure that entire passes of the dataflow engine 
 -- aren't postponed in a thunk. By making the sequence strict in the state,
 -- we ensure that each action in the monad is executed immediately, preventing
 -- stack overflows that previously occurred when finally forcing the old state thunks.

instance FuelUsingMonad (DFM' FuelMonad f) where
  fuelRemaining = liftToDFM' fuelRemaining
  lastFuelPass  = liftToDFM' lastFuelPass
  fuelExhausted = liftToDFM' fuelExhausted
  fuelDecrement p f f' = liftToDFM' (fuelDecrement p f f')
  fuelDec1      = liftToDFM' fuelDec1
instance MonadUnique (DFM' FuelMonad f) where
    getUniqueSupplyM = liftToDFM' getUniqueSupplyM
    getUniqueM       = liftToDFM' getUniqueM
    getUniquesM      = liftToDFM' getUniquesM

liftToDFM' :: Monad m => m x -> DFM' m f x
liftToDFM' m = DFM' (\ _ s -> m >>= (\a -> return (a, s)))
liftToDFM :: FuelMonad x -> DFM f x
liftToDFM m = DFM' (\ _ s -> m >>= (\a -> return (a, s)))


pprSetFact :: (Show a, Outputable f) => a -> f -> f -> f -> SDoc
pprSetFact id old a join =
    f4sep [text "at" <+> text (show id),
           text "added" <+> ppr a, text "to" <+> ppr old,
           text "yielding" <+> ppr join]

f4sep :: [SDoc] -> SDoc
f4sep [] = fsep []
f4sep (d:ds) = fsep (d : map (nest 4) ds)
