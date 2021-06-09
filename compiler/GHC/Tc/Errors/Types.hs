{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module GHC.Tc.Errors.Types (
  -- * Main types
    TcRnMessage(..)
  , ErrInfo(..)
  , LevityCheckProvenance(..)
  ) where

import GHC.Hs
import GHC.Types.Error
import GHC.Types.Name (Name)
import GHC.Types.Name.Reader
import GHC.Unit.Types (Module)
import GHC.Utils.Outputable
import Data.Typeable
import GHC.Core.Type (Type, Var)

-- The majority of TcRn messages come with extra context about the error,
-- and this newtype captures it.
newtype ErrInfo = ErrInfo { getErrInfo :: SDoc }

-- | An error which might arise during typechecking/renaming.
data TcRnMessage where
  {-| Simply wraps a generic 'Diagnostic' message @a@. It can be used by plugins
      to provide custom diagnostic messages originated during typechecking/renaming.
  -}
  TcRnUnknownMessage :: (Diagnostic a, Typeable a) => a -> TcRnMessage

  {-| A levity polymorphism check happening during TcRn.
  -}
  TcLevityPolyInType :: !Type
                     -> !LevityCheckProvenance
                     -> !ErrInfo -- Extra info accumulated in the TcM monad
                     -> TcRnMessage


  {-| TcRnImplicitLift is a warning (controlled with -Wimplicit-lift) that occurs when
      a Template Haskell quote implicitly uses 'lift'.

     Example:
       warning1 :: Lift t => t -> Q Exp
       warning1 x = [| x |]

     Test cases: th/T17804
  -}
  TcRnImplicitLift :: Outputable var => var -> !ErrInfo -> TcRnMessage
  {-| TcRnUnusedPatternBinds is a warning (controlled with -Wunused-pattern-binds)
      that occurs if a pattern binding binds no variables at all, unless it is a
      lone wild-card pattern, or a banged pattern.

     Example:
        Just _ = rhs3    -- Warning: unused pattern binding
        (_, _) = rhs4    -- Warning: unused pattern binding
        _  = rhs3        -- No warning: lone wild-card pattern
        !() = rhs4       -- No warning: banged pattern; behaves like seq

     Test cases: rename/{T13646,T17c,T17e,T7085}
  -}
  TcRnUnusedPatternBinds :: HsBind GhcRn -> TcRnMessage
  {-| TcRnDodgyImports is a warning (controlled with -Wdodgy-imports) that occurs when
      a datatype 'T' is imported with all constructors, i.e. 'T(..)', but has been exported
      abstractly, i.e. 'T'.

     Test cases: rename/should_compile/T7167
  -}
  TcRnDodgyImports :: RdrName -> TcRnMessage
  {-| TcRnDodgyExports is a warning (controlled by -Wdodgy-exports) that occurs when a datatype
      'T' is exported with all constructors, i.e. 'T(..)', but is it just a type synonym or a
      type/data family.

     Example:
       module Foo (
           T(..)  -- Warning: T is a type synonym
         , A(..)  -- Warning: A is a type family
         , C(..)  -- Warning: C is a data family
         ) where

       type T = Int
       type family A :: * -> *
       data family C :: * -> *

     Test cases: warnings/should_compile/DodgyExports01
  -}
  TcRnDodgyExports :: Name -> TcRnMessage
  {-| TcRnMissingImportList is a warning (controlled by -Wmissing-import-lists) that occurs when
      an import declaration does not explicitly list all the names brought into scope.

     Test cases: rename/should_compile/T4489
  -}
  TcRnMissingImportList :: IE GhcPs -> TcRnMessage
  {-| When a module marked trustworthy or unsafe (using -XTrustworthy or -XUnsafe) is compiled
      with a plugin, the TcRnUnsafeDueToPlugin warning (controlled by -Wunsafe) is used as the
      reason the module was inferred to be unsafe. This warning is not raised if the
      -fplugin-trustworthy flag is passed.
  -}
  TcRnUnsafeDueToPlugin :: TcRnMessage
  {-| TcRnModMissingRealSrcSpan is an error that occurrs when compiling a module that lacks
      an associated 'RealSrcSpan'.
  -}
  TcRnModMissingRealSrcSpan :: Module -> TcRnMessage


-- | Where the levity checking for the input type originated
data LevityCheckProvenance
  = LevityCheckInVarType
  | LevityCheckInBinder !Var
  | LevityCheckInWildcardPattern
  | LevityCheckInUnboxedTuplePattern !(Pat GhcTc)
  | LevityCheckPatSynSig
  | LevityCheckCmdStmt
  | LevityCheckMkCmdEnv !Var
  | LevityCheckDoCmd !(HsCmd GhcTc)
  | LevityCheckDesugaringCmd !(LHsCmd GhcTc)
  | LevityCheckInCmd !(LHsCmd GhcTc)
  | LevityCheckInFunUse !(LHsExpr GhcTc)
  | LevityCheckInValidDataCon
  | LevityCheckInValidClass

