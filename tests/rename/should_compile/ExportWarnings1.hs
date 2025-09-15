-- Case for explicit mentions of imports
module ExportWarnings1 () where

import ExportWarnings_aux (x, S(S1))
import ExportWarnings_base (x, T(T1), V, B)