-- !!! check that hiding works with qualified imports
module ShouldFail ( List.map ) where
import qualified List hiding ( map )
