{-# OPTIONS_GHC -Wno-orphans #-} -- Outputable, Binary, Eq
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | Fixity
module GHC.Types.Fixity
   ( Fixity (..)
   , FixityDirection (..)
   , LexicalFixity (..)
   , maxPrecedence
   , minPrecedence
   , defaultFixity
   , negateFixity
   , funTyFixity
   , compareFixity
   , fixityFromSyntax
   , fixityToSyntax
   )
where

import GHC.Prelude

import GHC.Hs.Extension
import qualified GHC.Hs.Basic as H
import GHC.Types.SourceText

import GHC.Utils.Outputable
import GHC.Utils.Binary

import Data.Data hiding (Fixity(..))

import Language.Haskell.Syntax.Extension
import Language.Haskell.Syntax.Basic (FixityDirection(..), LexicalFixity(..))

type instance XFixity (GhcPass _) = SourceText -- Note [Pragma source text]
type instance XXFixity (GhcPass _) = DataConCantHappen

-- | Fixity used internally in GHC, so that we don't have to take `GhcPass p`
-- everywhere.
--
-- The Fixity defined in the AST is converted to this Fixity
--
-- See `fixityFromSyntax`
data Fixity = Fixity Int FixityDirection
    deriving (Eq, Data)

instance Outputable Fixity where
    ppr (Fixity prec dir) = hcat [ppr dir, space, int prec]

instance Binary Fixity where
    put_ bh (Fixity aa ab) = do
            put_ bh aa
            put_ bh ab
    get bh = do
          aa <- get bh
          ab <- get bh
          return (Fixity aa ab)

------------------------
maxPrecedence, minPrecedence :: Int
maxPrecedence = 9
minPrecedence = 0

defaultFixity :: Fixity
defaultFixity = Fixity maxPrecedence InfixL

negateFixity, funTyFixity :: Fixity
-- Wired-in fixities
negateFixity = Fixity 6 InfixL  -- Fixity of unary negate
funTyFixity  = Fixity (-1) InfixR  -- Fixity of '->', see #15235

{-
Consider

\begin{verbatim}
        a `op1` b `op2` c
\end{verbatim}
@(compareFixity op1 op2)@ tells which way to arrange application, or
whether there's an error.
-}

compareFixity :: Fixity -> Fixity
              -> (Bool,         -- Error please
                  Bool)         -- Associate to the right: a op1 (b op2 c)
compareFixity (Fixity prec1 dir1) (Fixity prec2 dir2)
  = case prec1 `compare` prec2 of
        GT -> left
        LT -> right
        EQ -> case (dir1, dir2) of
                        (InfixR, InfixR) -> right
                        (InfixL, InfixL) -> left
                        _                -> error_please
  where
    right        = (False, True)
    left         = (False, False)
    error_please = (True,  False)

fixityFromSyntax :: H.Fixity (GhcPass p) -> Fixity
fixityFromSyntax (H.Fixity _ i d) = Fixity i d

fixityToSyntax :: Fixity -> H.Fixity (GhcPass p)
fixityToSyntax (Fixity i d) = H.Fixity NoSourceText i d
