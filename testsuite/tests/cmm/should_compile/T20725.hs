module T20725 where

import GHC.Cmm
import GHC.Cmm.Dataflow
import GHC.Cmm.Dataflow.Block
import GHC.Cmm.Dataflow.Graph
import GHC.Cmm.Dataflow.Label

data TrivialFact = TrivialFact

trivialLattice :: DataflowLattice TrivialFact
trivialLattice = DataflowLattice TrivialFact (\_ _ -> NotChanged TrivialFact)

-- | ensures that analysis is possible independent of node type
trivialMap :: forall node .
              (NonLocal node)
           => GenCmmGraph node
           -> LabelMap TrivialFact
trivialMap g =
  analyzeCmmFwd trivialLattice transfer g startFacts
      where startFacts = mkFactBase trivialLattice []
            transfer block facts =
                asBase [(successor, TrivialFact) | successor <- successors block]
                    where asBase = mkFactBase trivialLattice
