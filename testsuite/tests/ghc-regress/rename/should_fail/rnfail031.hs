-- !!! check that qualified imports can be restricted to certain names
module ShouldFail ( Data.List.map ) where
import qualified Data.List ( foldr )
