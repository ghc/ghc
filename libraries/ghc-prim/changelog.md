## 0.5.0.0

- `GHC.Classes`: new `class IP (a :: Symbol) b | a -> b`

- `GHC.Prim`: changed type signatures from

        check# :: (State# RealWorld -> (# State# RealWorld, a #)) -> State# RealWorld -> (# State# RealWorld, () #)
        finalizeWeak# :: Weak# a -> State# RealWorld -> (# State# RealWorld, Int#, State# RealWorld -> (# State# RealWorld, () #) #)
        mkWeak# :: a -> b -> c -> State# RealWorld -> (# State# RealWorld, Weak# b #)

    to

        check# :: (State# RealWorld -> (# State# RealWorld, a #)) -> State# RealWorld -> State# RealWorld
        finalizeWeak# :: Weak# a -> State# RealWorld -> (# State# RealWorld, Int#, State# RealWorld -> (# State# RealWorld, b #) #)
        mkWeak# :: a -> b -> (State# RealWorld -> (# State# RealWorld, c #)) -> State# RealWorld -> (# State# RealWorld, Weak# b #)

- Removed from `GHC.Prim`:

        parAt# :: b -> a -> Int# -> Int# -> Int# -> Int# -> c -> Int#
        parAtAbs# :: a -> Int# -> Int# -> Int# -> Int# -> Int# -> b -> Int#
        parAtForNow# :: b -> a -> Int# -> Int# -> Int# -> Int# -> c -> Int#
        parAtRel# :: a -> Int# -> Int# -> Int# -> Int# -> Int# -> b -> Int#
        parGlobal# :: a -> Int# -> Int# -> Int# -> Int# -> b -> Int#
        parLocal# :: a -> Int# -> Int# -> Int# -> Int# -> b -> Int#

- Added to `GHC.Prim`:

        getSizeofMutableByteArray# :: MutableByteArray# d -> State# d -> (# State# d, Int# #)
        subWordC# :: Word# -> Word# -> (# Word#, Int# #)
        runRW# :: (State# RealWorld -> (# State# RealWorld, o #)) -> (# State# RealWorld, o #)

- Added to `GHC.Types`:

        data Module = Module TrName TrName
        data Nat
        data Symbol
        data TrName = TrNameS Addr# | TrNameD [Char]
        data TyCon = TyCon Word# Word# Module TrName
