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
   )
where

import GHC.Prelude

import GHC.Types.SourceText

import GHC.Utils.Outputable
import GHC.Utils.Binary

import Data.Data hiding (Fixity, Prefix, Infix)

data Fixity = Fixity SourceText Int FixityDirection
  -- Note [Pragma source text]
  deriving Data

instance Outputable Fixity where
    ppr (Fixity _ prec dir) = hcat [ppr dir, space, int prec]

instance Eq Fixity where -- Used to determine if two fixities conflict
  (Fixity _ p1 dir1) == (Fixity _ p2 dir2) = p1==p2 && dir1 == dir2

instance Binary Fixity where
    put_ bh (Fixity src aa ab) = do
            put_ bh src
            put_ bh aa
            put_ bh ab
    get bh = do
          src <- get bh
          aa <- get bh
          ab <- get bh
          return (Fixity src aa ab)

------------------------
data FixityDirection
   = InfixL
   | InfixR
   | InfixN
   deriving (Eq, Data)

instance Outputable FixityDirection where
    ppr InfixL = text "infixl"
    ppr InfixR = text "infixr"
    ppr InfixN = text "infix"

instance Binary FixityDirection where
    put_ bh InfixL =
            putByte bh 0
    put_ bh InfixR =
            putByte bh 1
    put_ bh InfixN =
            putByte bh 2
    get bh = do
            h <- getByte bh
            case h of
              0 -> return InfixL
              1 -> return InfixR
              _ -> return InfixN

------------------------
maxPrecedence, minPrecedence :: Int
maxPrecedence = 9
minPrecedence = 0

defaultFixity :: Fixity
defaultFixity = Fixity NoSourceText maxPrecedence InfixL

negateFixity, funTyFixity :: Fixity
-- Wired-in fixities
negateFixity = Fixity NoSourceText 6 InfixL  -- Fixity of unary negate
funTyFixity  = Fixity NoSourceText (-1) InfixR  -- Fixity of '->', see #15235

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
compareFixity (Fixity _ prec1 dir1) (Fixity _ prec2 dir2)
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

-- |Captures the fixity of declarations as they are parsed. This is not
-- necessarily the same as the fixity declaration, as the normal fixity may be
-- overridden using parens or backticks.
data LexicalFixity = Prefix | Infix deriving (Data,Eq)

instance Outputable LexicalFixity where
  ppr Prefix = text "Prefix"
  ppr Infix  = text "Infix"
