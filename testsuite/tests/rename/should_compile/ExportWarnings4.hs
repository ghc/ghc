-- Case for when not all import paths are deprecated but the name is qualified
module ExportWarnings4 () where

import ExportWarnings_base
import ExportWarnings_aux
foo = ExportWarnings_aux.x