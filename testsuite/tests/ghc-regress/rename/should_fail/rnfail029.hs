-- !!! conflicting exports for a function name
module ShouldFail ( List.map, module ShouldFail ) where
import qualified List
map = undefined
