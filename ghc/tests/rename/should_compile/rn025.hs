-- !!! Re-exporting a module whose contents is partially hidden.
module ShouldSucceed ( module List ) where

import List hiding ( sort )

