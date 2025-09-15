-- Case for when names are mentioned in the hiding clauses
module ExportWarnings5 () where

import ExportWarnings_aux hiding (x, S(..))
import ExportWarnings_base hiding (x, T(T2, y), V)