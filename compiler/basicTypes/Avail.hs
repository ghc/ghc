--
-- (c) The University of Glasgow
--

module Avail (
    Avails,
    AvailInfo(..),
    availsToNameSet,
    availsToNameEnv,
    availName, availNames,
    stableAvailCmp,

    NameWarn(..),
    nameWarnName,
    availNameWarns,
  ) where

import Name
import NameEnv
import NameSet

import BasicTypes
import Binary
import Outputable
import Util

-- -----------------------------------------------------------------------------
-- The NameWarn type

data NameWarn = NameWarn Name (Maybe WarningTxt)

nameWarnName :: NameWarn -> Name
nameWarnName (NameWarn n _) = n

-- XXX?
instance Eq NameWarn where
    x == y = nameWarnName x == nameWarnName y

instance Outputable NameWarn where
    ppr (NameWarn n m) = ppr n <> braces wd
        where wd = case m of
                   Nothing -> text "no warning"
                   Just w -> text "warning:" <+> ppr w

instance Binary NameWarn where
    put_ h (NameWarn n w) = do put_ h n
                               put_ h w
    get h = do n <- get h
               w <- get h
               return (NameWarn n w)

-- -----------------------------------------------------------------------------
-- The AvailInfo type

-- | Records what things are "available", i.e. in scope
data AvailInfo = Avail NameWarn  -- ^ An ordinary identifier in scope
               | AvailTC NameWarn
                     [NameWarn]  -- ^ A type or class in scope. Parameters:
                                 --
                                 --  1) The name of the type or class
                                 --  2) The available pieces of type or class.
                                 --
                                 -- The AvailTC Invariant:
                                 --   * If the type or class is itself
                                 --     to be in scope, it must be
                                 --     *first* in this list.  Thus,
                                 --     typically: @AvailTC Eq [Eq, ==, \/=]@
                deriving( Eq )
                        -- Equality used when deciding if the
                        -- interface has changed

-- | A collection of 'AvailInfo' - several things that are \"available\"
type Avails = [AvailInfo]

-- | Compare lexicographically
stableAvailCmp :: AvailInfo -> AvailInfo -> Ordering
stableAvailCmp (Avail n1)     (Avail n2)     = nameWarnName n1 `stableNameCmp`
                                               nameWarnName n2
stableAvailCmp (Avail {})     (AvailTC {})   = LT
stableAvailCmp (AvailTC n ns) (AvailTC m ms) = (nameWarnName n `stableNameCmp`
                                                nameWarnName m) `thenCmp`
                                               (cmpList stableNameCmp
                                                        (map nameWarnName ns)
                                                        (map nameWarnName ms))
stableAvailCmp (AvailTC {})   (Avail {})     = GT


-- -----------------------------------------------------------------------------
-- Operations on AvailInfo

availsToNameSet :: [AvailInfo] -> NameSet
availsToNameSet avails = foldr add emptyNameSet avails
      where add avail set = extendNameSetList set (availNames avail)

availsToNameEnv :: [AvailInfo] -> NameEnv AvailInfo
availsToNameEnv avails = foldr add emptyNameEnv avails
     where add avail env = extendNameEnvList env
                                (zip (availNames avail) (repeat avail))

-- | Just the main name made available, i.e. not the available pieces
-- of type or class brought into scope by the 'GenAvailInfo'
availName :: AvailInfo -> Name
availName = nameWarnName . availNameWarn

availNameWarn :: AvailInfo -> NameWarn
availNameWarn (Avail nw)     = nw
availNameWarn (AvailTC nw _) = nw

-- | All names made available by the availability information
availNames :: AvailInfo -> [Name]
availNames = map nameWarnName . availNameWarns

availNameWarns :: AvailInfo -> [NameWarn]
availNameWarns (Avail nw)      = [nw]
availNameWarns (AvailTC _ nws) = nws

-- -----------------------------------------------------------------------------
-- Printing

instance Outputable AvailInfo where
   ppr = pprAvail

pprAvail :: AvailInfo -> SDoc
pprAvail (Avail n)      = ppr n
pprAvail (AvailTC n ns) = ppr n <> braces (hsep (punctuate comma (map ppr ns)))

instance Binary AvailInfo where
    put_ bh (Avail aa) = do
            putByte bh 0
            put_ bh aa
    put_ bh (AvailTC ab ac) = do
            putByte bh 1
            put_ bh ab
            put_ bh ac
    get bh = do
            h <- getByte bh
            case h of
              0 -> do aa <- get bh
                      return (Avail aa)
              _ -> do ab <- get bh
                      ac <- get bh
                      return (AvailTC ab ac)
