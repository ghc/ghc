-- |This modules bundles all vectorised versions of Prelude definitions.
--
--  /This module should not be explicitly imported in user code anymore./
--  User code should only import 'Data.Array.Parallel' and, until the
--  vectoriser supports type classes, the type-specific
--  modules 'Data.Array.Parallel.Prelude.*'.

module Data.Array.Parallel.Prelude 
        ( module Data.Array.Parallel.Prelude.Base
        , module Data.Array.Parallel.Prelude.Bool)
where
import Data.Array.Parallel.Prelude.Base
import Data.Array.Parallel.Prelude.Bool
import Data.Array.Parallel.Prelude.Ordering ()
import Data.Array.Parallel.Prelude.Int      ()
import Data.Array.Parallel.Prelude.Word8    ()
import Data.Array.Parallel.Prelude.Double   ()
