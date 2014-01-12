-- !!! conflicting exports for a function name
module ShouldFail ( Data.List.map, module ShouldFail ) where
import qualified Data.List
map = undefined
