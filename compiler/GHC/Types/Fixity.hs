{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-} -- For re-export of GHC.Hs.Basic instances

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
   , module GHC.Hs.Basic
   )
where

import GHC.Prelude

import Language.Haskell.Syntax.Basic (LexicalFixity(..), FixityDirection(..), Fixity(..) )
import GHC.Hs.Basic () -- For instances only

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
