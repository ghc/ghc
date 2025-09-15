import qualified P.Lazy
import qualified P.Strict
main = P.Lazy.foo >> P.Strict.foo
