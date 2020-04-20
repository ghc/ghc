-----------------------------------------------------------------------------
-- A Parser monad with access to the 'DynFlags'.
--
-- The 'P' monad  only has access to the subset of of 'DynFlags'
-- required for parsing Haskell.

-- The parser for C-- requires access to a lot more of the 'DynFlags',
-- so 'PD' provides access to 'DynFlags' via a 'HasDynFlags' instance.
-----------------------------------------------------------------------------
module GHC.Cmm.Monad (
    PD(..)
  , liftP
  , failMsgPD
  ) where

import GHC.Prelude

import Control.Monad

import GHC.Driver.Session
import GHC.Parser.Lexer

newtype PD a = PD { unPD :: DynFlags -> PState -> ParseResult a }

instance Functor PD where
  fmap = liftM

instance Applicative PD where
  pure = returnPD
  (<*>) = ap

instance Monad PD where
  (>>=) = thenPD

liftP :: P a -> PD a
liftP (P f) = PD $ \_ s -> f s

failMsgPD :: String -> PD a
failMsgPD = liftP . failMsgP

returnPD :: a -> PD a
returnPD = liftP . return

thenPD :: PD a -> (a -> PD b) -> PD b
(PD m) `thenPD` k = PD $ \d s ->
        case m d s of
                POk s1 a         -> unPD (k a) d s1
                PFailed s1 -> PFailed s1

instance HasDynFlags PD where
   getDynFlags = PD $ \d s -> POk s d
