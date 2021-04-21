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
  , getProfile
  , getPlatform
  , getPtrOpts
  , getHomeUnitId
  ) where

import GHC.Prelude

import GHC.Platform
import GHC.Platform.Profile
import GHC.Cmm.Info

import Control.Monad

import GHC.Driver.Session
import GHC.Parser.Lexer
import GHC.Parser.Errors.Types
import GHC.Types.Error ( MsgEnvelope )
import GHC.Types.SrcLoc
import GHC.Unit.Types
import GHC.Unit.Home

newtype PD a = PD { unPD :: DynFlags -> HomeUnit -> PState -> ParseResult a }

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

instance HasDynFlags PD where
   getDynFlags = PD $ \d _ s -> POk s d

getProfile :: PD Profile
getProfile = targetProfile <$> getDynFlags

getPlatform :: PD Platform
getPlatform = profilePlatform <$> getProfile

getPtrOpts :: PD PtrOpts
getPtrOpts = do
   dflags <- getDynFlags
   profile <- getProfile
   pure $ PtrOpts
      { po_profile     = profile
      , po_align_check = gopt Opt_AlignmentSanitisation dflags
      }

-- | Return the UnitId of the home-unit. This is used to create labels.
getHomeUnitId :: PD UnitId
getHomeUnitId = PD $ \_ hu s -> POk s (homeUnitId hu)
