-- !!! check that hiding on an unqualified import also hides the qualified name
module ShouldFail ( Data.List.map ) where
import Data.List hiding ( map )
