-- !!! check that unqualified imports don't bring qualified names into scope
module ShouldFail ( List.map ) where
import List ()
