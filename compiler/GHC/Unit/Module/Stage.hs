module GHC.Unit.Module.Stage ( ModuleStage(..)
                             , allStages
                             , nowAndFutureStages
                             , onlyFutureStages
                             , minStage
                             , maxStage
                             , zeroStage
                             , decModuleStage
                             , incModuleStage
                             ) where

import GHC.Prelude
import GHC.Utils.Outputable

{- Note [Stage vs Level]
~~~~~~~~~~~~~~~~~~~~~~~~

Modules are compiled at a specific stage. Levels within a module are interpreted
as offsets to the specific stage at which the module is being compiled.

* A **level** is a typechecking concept. The type checker performs level checking
  to ensure that the evaluation can proceed in a well-staged manner.
* A **stage** is an operational construct. The execution of the program happens
  in stages.

GHC at the moment knows about two stages, a module is either compiled for
compile time (*C*) or runtime (*R*), with *C* before *R*. Then:

* The main module is compiled for `R`.

* A normal import does not shift the stage at which the dependent module is required.

* If a module `M` splice imports module `A`, then compiling `M` at stage
  *R* requires compiling module `A` at stage *C*.

* If a module `N` quote imports module `B`, then compiling `N` at stage
  *C* requires compiling module `B` at stage *R*.

The compiler can then choose appropiately how modules needed at `C` are compiled
and how modules needed at `R` are compiled.

For example:

* In `-fno-code` mode, `C` modules may be compiled in dynamic way, but `R` modules
  are not compiled at all.
* When using a profiled GHC. `C` modules must be compiled in profiled way but `R` modules
  will be compiled in static way.

Further structure as needed by cross-compilation settings may require more stages.

-}

-- The order of these constructors is important for definitions such as
-- 'futureStages'.
data ModuleStage = CompileStage | RunStage deriving (Eq, Ord, Enum, Bounded)

allStages :: [ModuleStage]
allStages = [minBound .. maxBound]

nowAndFutureStages :: ModuleStage -> [ModuleStage]
nowAndFutureStages cur_st = [cur_st .. ]

onlyFutureStages :: ModuleStage -> [ModuleStage]
onlyFutureStages cur_st | cur_st == maxBound = []
onlyFutureStages cur_st = [succ cur_st .. ]

minStage :: ModuleStage
minStage = minBound

maxStage :: ModuleStage
maxStage = maxBound

instance Outputable ModuleStage where
  ppr CompileStage = text "compile"
  ppr RunStage = text "run"

zeroStage :: ModuleStage
zeroStage = RunStage

decModuleStage, incModuleStage :: ModuleStage -> ModuleStage
incModuleStage RunStage = RunStage
incModuleStage CompileStage = RunStage

decModuleStage RunStage = CompileStage
decModuleStage CompileStage = RunStage
