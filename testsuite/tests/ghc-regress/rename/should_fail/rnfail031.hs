-- !!! check that qualified imports can be restricted to certain names
module ShouldFail ( List.map ) where
import qualified List ( foldr )
