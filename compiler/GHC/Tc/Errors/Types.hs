{-# LANGUAGE GADTs #-}
module GHC.Tc.Errors.Types (
  -- * Main types
    TcRnMessage(..)
  , TcRnDsMessage(..)
  , ErrInfo(..)
  -- * Smart constructors
  , mkTcRnDsMessage
  ) where

import GHC.Hs
import GHC.HsToCore.Errors.Types
import GHC.Prelude
import GHC.Types.Error
import GHC.Types.Name (Name)
import GHC.Types.Name.Reader
import GHC.Utils.Outputable


{- Note [TcRnDsMessage]
~~~~~~~~~~~~~~~~~~~~~~~

There is a mutual dependency between typecheck-rename and desugaring,
because we need to desugar expressions during TH splicing. This means that
while we are in the 'TcM' monad, we could get 'DsMessage'(s), and we also
need to run a desugar action inside the 'TcM' monad (see 'initDsTc'). However,
we still need to accumulate diagnostics from both phases, and therefore GHC
uses the same /shared/ IORef between these two monads. This is why we introduce
the 'TcRnDsMessage': we still want to grab onto the full accumulated diagnostics
(without leaving anything behind) but we also need a single, monomorphic type to
use in the 'IORef'.

-}


-- | Captures both desugar and typecheck errors.
-- See Note [TcRnDsMessage]
newtype TcRnDsMessage = TcRnDsMessage (Either DsMessage TcRnMessage)

mkTcRnDsMessage :: Either DsMessage TcRnMessage -> TcRnDsMessage
mkTcRnDsMessage = TcRnDsMessage

-- The majority of TcRn messages comes with extra context about the error,
-- and this newtype captures it.
newtype ErrInfo = ErrInfo { getErrInfo :: SDoc }

-- | An error which might arise during typechecking/renaming.
data TcRnMessage where
  -- | Simply rewraps a generic 'DiagnosticMessage'. More
  -- instances will be added in the future (#18516).
  TcRnUnknownMessage :: !DiagnosticMessage -> TcRnMessage
  {-| TcRnImplicitLift occurs when when a Template Haskell quote implicitly uses 'lift'.

     Example:
       warning1 :: Lift t => t -> Q Exp
       warning1 x = [| x |]

     Test cases: th/T17804
  -}
  TcRnImplicitLift :: Outputable var => var -> !ErrInfo -> TcRnMessage
  {-| TcRnUnusedPatternBinds occurs when when some pattern match bindings are unused

     Example:
       foo :: IO ()
       foo = do let !() = assert False ()
                    -- Should not give a warning
                let () = assert False ()
                    -- Should give a warning
                pure ()

     Test cases: rename/{T13646,T17c,T17e,T7085}
  -}
  TcRnUnusedPatternBinds :: HsBind GhcRn -> TcRnMessage
  {-| TcRnDodgyImports occurs when a datatype 'T' is imported with all constructors, i.e.
       'T(..)', but has been exported abstractly, i.e. 'T'. It's also emitted when an 'import' statement
       hides an entity that is not exported.

     Test cases: rename/should_compile/T7167
  -}
  TcRnDodgyImports :: RdrName -> TcRnMessage
  {-| TcRnDodgyExports occurs when a datatype 'T' is exported with all constructors,
      i.e. 'T(..)', but is it just a type synonym. Also emitted when a module is re-exported,
      but that module exports nothing.

     Example:
       module Foo (T(..)) where

       type T = Int

     Test cases: warnings/should_compile/DodgyExports01
  -}
  TcRnDodgyExports :: Name -> TcRnMessage
  {-| TcRnMissingImportList occurs when an import declaration does not explicitly list all the
        names brought into scope.

     Test cases: rename/should_compile/T4489
  -}
  TcRnMissingImportList :: IE GhcPs -> TcRnMessage
