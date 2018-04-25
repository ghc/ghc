{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Distribution.Types.ExecutableScope (
    ExecutableScope(..),
) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Pretty
import Distribution.Parsec.Class
import Distribution.Text

import qualified Distribution.Compat.Parsec as P
import qualified Distribution.Compat.ReadP as Parse
import qualified Text.PrettyPrint as Disp

data ExecutableScope = ExecutableScopeUnknown
                     | ExecutablePublic
                     | ExecutablePrivate
    deriving (Generic, Show, Read, Eq, Typeable, Data)

instance Pretty ExecutableScope where
    pretty ExecutablePublic       = Disp.text "public"
    pretty ExecutablePrivate      = Disp.text "private"
    pretty ExecutableScopeUnknown = Disp.text "unknown"

instance Parsec ExecutableScope where
    parsec = do
        name <- P.munch1 (\c -> isAlphaNum c || c == '-')
        return $ case name of
              "public"  -> ExecutablePublic
              "private" -> ExecutablePrivate
              _         -> ExecutableScopeUnknown

instance Text ExecutableScope where
    parse = Parse.choice
        [ Parse.string "public"  >> return ExecutablePublic
        , Parse.string "private" >> return ExecutablePrivate
        ]

instance Binary ExecutableScope

instance Monoid ExecutableScope where
    mempty = ExecutableScopeUnknown
    mappend = (<>)

instance Semigroup ExecutableScope where
    ExecutableScopeUnknown <> x = x
    x <> ExecutableScopeUnknown = x
    x <> y | x == y             = x
           | otherwise          = error "Ambiguous executable scope"
