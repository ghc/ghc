-- We don't want to see a case match on the result
-- of dataToTag#. We check that by looking for case
-- matches on any literal other than 0# or 1#. We
-- only really expect to see 1# here (and not the tag
-- for OtherOS), but just in case GHC decides to get
-- clever with its knowledge of the possible results
-- of (==#), we also permit 0#.
{-# LANGUAGE MagicHash, BangPatterns #-}
module T14140 where
import GHC.Exts

data OS = Linux | Windows | OSX        -- tier 1 desktop OSs
        | FreeBSD | OpenBSD | NetBSD   -- other free Unix OSs
        | DragonFly
        | Solaris | AIX | HPUX | IRIX  -- ageing Unix OSs
        | HaLVM                        -- bare metal / VMs / hypervisors
        | Hurd                         -- GNU's microkernel
        | IOS  | Android               -- mobile OSs
        | Ghcjs
        | OtherOS String

-- This is similar to the derived Eq instance. We write it
-- out manually to prevent future deriving changes from
-- changing the test.
instance Eq OS where
  OtherOS x == OtherOS y = x == y
  (!x) == (!y) = case dataToTag# x ==# dataToTag# y of
               1# -> True
               _ -> False
