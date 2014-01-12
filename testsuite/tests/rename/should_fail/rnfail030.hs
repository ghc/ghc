-- !!! check that unqualified imports don't bring qualified names into scope
module ShouldFail ( Data.List.map ) where
import Data.List ()
