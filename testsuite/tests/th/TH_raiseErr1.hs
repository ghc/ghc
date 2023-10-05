module TH_raiseErr1 where
import Language.Haskell.TH

foo = $(do { report True "Error test succeeded"; fail "" })
