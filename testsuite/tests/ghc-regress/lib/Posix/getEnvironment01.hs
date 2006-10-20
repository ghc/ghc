
-- test for trac #781 (GHCi on x86_64, cannot link to static data in
-- shared libs)

import System.Posix.Env

main = getEnvironment >>= (print . (0 <=) . length)

