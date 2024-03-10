{-# LANGUAGE NoImplicitPrelude, StandaloneDeriving #-}
{-# OPTIONS_HADDOCK not-home #-}

-- | Maybe type
module GHC.Internal.Maybe
   ( Maybe (..)
   )
where

-- See W1 of Note [Tracking dependencies on primitives] in GHC.Internal.Base
import GHC.Classes

default ()

-------------------------------------------------------------------------------
-- Maybe type
-------------------------------------------------------------------------------

-- | The 'Maybe' type encapsulates an optional value.  A value of type
-- @'Maybe' a@ either contains a value of type @a@ (represented as @'Just' a@),
-- or it is empty (represented as 'Nothing').  Using 'Maybe' is a good way to
-- deal with errors or exceptional cases without resorting to drastic
-- measures such as 'Prelude.error'.
--
-- The 'Maybe' type is also a monad.  It is a simple kind of error
-- monad, where all errors are represented by 'Nothing'.  A richer
-- error monad can be built using the 'Data.Either.Either' type.
--
data  Maybe a  =  Nothing | Just a
  deriving ( Eq  -- ^ @since base-2.01

           --, Ord -- ^ @since base-2.01
           )

-- ???
-- A non-standalone instance will slurp the interface file for GHC.Num.Integer.
  -- During simplifyInstanceContexts, a call to GHC.Tc.Utils.Env.tcGetDefaultTys
  -- apparently sees mb_defaults = Nothing and thus tries to bring in the
  -- default "default" types, including Integer.  This seems wrong.
deriving instance Ord a => Ord (Maybe a) -- ^ @since base-2.01

