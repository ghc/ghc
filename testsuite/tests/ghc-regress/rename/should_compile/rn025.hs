-- !!! Re-exporting a module whose contents is partially hidden.
module ShouldCompile ( module List ) where

import List hiding ( sort )

