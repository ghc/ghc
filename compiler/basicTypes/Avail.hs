--
-- (c) The University of Glasgow
--

module Avail (
    Avails,
    AvailInfo(..),
    availsToNameSet,
    availsToNameEnv,
    availName, availNames,
    stableAvailCmp
  ) where

import Name
import NameEnv
import NameSet

import Binary
import Outputable
import Util

-- -----------------------------------------------------------------------------
-- The AvailInfo type

-- | Records what things are "available", i.e. in scope
data AvailInfo = Avail Name      -- ^ An ordinary identifier in scope
               | AvailTC Name
                         [Name]  -- ^ A type or class in scope. Parameters:
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
stableAvailCmp (Avail n1)     (Avail n2)     = n1 `stableNameCmp` n2
stableAvailCmp (Avail {})     (AvailTC {})   = LT
stableAvailCmp (AvailTC n ns) (AvailTC m ms) = (n `stableNameCmp` m) `thenCmp`
                                               (cmpList stableNameCmp ns ms)
stableAvailCmp (AvailTC {})   (Avail {})     = GT


-- -----------------------------------------------------------------------------
-- Operations on AvailInfo

availsToNameSet :: [AvailInfo] -> NameSet
availsToNameSet avails = foldr add emptyNameSet avails
      where add avail set = addListToNameSet set (availNames avail)

availsToNameEnv :: [AvailInfo] -> NameEnv AvailInfo
availsToNameEnv avails = foldr add emptyNameEnv avails
     where add avail env = extendNameEnvList env
                                (zip (availNames avail) (repeat avail))

-- | Just the main name made available, i.e. not the available pieces
-- of type or class brought into scope by the 'GenAvailInfo'
availName :: AvailInfo -> Name
availName (Avail n)     = n
availName (AvailTC n _) = n

-- | All names made available by the availability information
availNames :: AvailInfo -> [Name]
availNames (Avail n)      = [n]
availNames (AvailTC _ ns) = ns

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

