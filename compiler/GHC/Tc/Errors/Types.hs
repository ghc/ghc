{-# LANGUAGE GADTs #-}
module GHC.Tc.Errors.Types (
  -- * Main types
    TcRnMessage(..)
  , ErrInfo(..)
  ) where

import GHC.Hs
import GHC.Types.Error
import GHC.Types.Name (Name)
import GHC.Types.Name.Reader
import GHC.Utils.Outputable
import Data.Typeable

-- The majority of TcRn messages come with extra context about the error,
-- and this newtype captures it.
newtype ErrInfo = ErrInfo { getErrInfo :: SDoc }

-- | An error which might arise during typechecking/renaming.
data TcRnMessage where
  {-| Simply wraps a generic 'Diagnostic' message @a@. It can be used by plugins
      to provide custom diagnostic messages originated during typechecking/renaming.
  -}
  TcRnUnknownMessage :: (Diagnostic a, Typeable a) => a -> TcRnMessage
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
