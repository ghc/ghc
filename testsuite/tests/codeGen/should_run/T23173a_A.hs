module T23173a_A where

-- A statically evaluated constructor value. Its interface must carry LFCon
-- even at -O0 (where -fomit-interface-pragmas is on), so importers tag
-- references to it. See Note [Pointer tagging of unlifted boxed primitives]
-- in GHC.StgToCmm.Prim and mkFullIface in GHC.Iface.Make.
x :: Maybe Bool
x = Just True
