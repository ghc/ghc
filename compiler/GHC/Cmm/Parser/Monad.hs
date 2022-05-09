-----------------------------------------------------------------------------
-- A Parser monad with access to the 'DynFlags'.
--
-- The 'P' monad only has access to the subset of 'DynFlags'
-- required for parsing Haskell.

-- The parser for C-- requires access to a lot more of the 'DynFlags',
-- so 'PD' provides access to 'DynFlags' via a 'HasDynFlags' instance.
-----------------------------------------------------------------------------
module GHC.Cmm.Parser.Monad (
    PD(..)
  , liftP
  , failMsgPD
  , getPDConfig
  , getProfile
  , getPlatform
  , getHomeUnitId
  , PDConfig(..)
  ) where

import GHC.Prelude

import GHC.Cmm.Parser.Config

import GHC.Platform
import GHC.Platform.Profile

import Control.Monad

import GHC.Parser.Lexer
import GHC.Parser.Errors.Types
import GHC.Types.Error ( MsgEnvelope )
import GHC.Types.SrcLoc
import GHC.Unit.Types
import GHC.Unit.Home

newtype PD a = PD { unPD :: PDConfig -> HomeUnit -> PState -> ParseResult a }

instance Functor PD where
  fmap = liftM

instance Applicative PD where
  pure = returnPD
  (<*>) = ap

instance Monad PD where
  (>>=) = thenPD

liftP :: P a -> PD a
liftP (P f) = PD $ \_ _ s -> f s

failMsgPD :: (SrcSpan -> MsgEnvelope PsMessage) -> PD a
failMsgPD = liftP . failMsgP

returnPD :: a -> PD a
returnPD = liftP . return

thenPD :: PD a -> (a -> PD b) -> PD b
(PD m) `thenPD` k = PD $ \d hu s ->
        case m d hu s of
                POk s1 a   -> unPD (k a) d hu s1
                PFailed s1 -> PFailed s1

getPDConfig :: PD PDConfig
getPDConfig = PD $ \pdc _ s -> POk s pdc

getProfile :: PD Profile
getProfile = PD $ \pdc _ s -> POk s (pdProfile pdc)

getPlatform :: PD Platform
getPlatform = profilePlatform <$> getProfile

-- | Return the UnitId of the home-unit. This is used to create labels.
getHomeUnitId :: PD UnitId
getHomeUnitId = PD $ \_ hu s -> POk s (homeUnitId hu)
