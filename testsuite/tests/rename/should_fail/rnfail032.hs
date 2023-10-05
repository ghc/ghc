-- !!! check that hiding works with qualified imports
module ShouldFail ( Data.List.map ) where
import qualified Data.List hiding ( map )
