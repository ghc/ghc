-- !!! check that hiding on an unqualified import also hides the qualified name
module ShouldFail ( List.map ) where
import List hiding ( map )
