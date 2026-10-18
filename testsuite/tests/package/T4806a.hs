import Data.Map

-- CHECK:      T4806a.hs:1:1: error: [GHC-87110]
-- CHECK-NEXT: Could not load module ‘Data.Map’.
-- CHECK-NEXT: It is a member of the package ‘containers-{{[0-9.]+}}’
-- CHECK-NEXT: which is unusable because the -ignore-package flag was used to ignore at least one of its dependencies:
-- CHECK-NEXT: deepseq-{{[0-9.]+}}
