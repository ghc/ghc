---------------------------------------------------------------------------
-- 				GHC/Prim.hi-boot
-- 
-- 	This hand-written interface file allows you to bring into scope the 
--	primitive operations and types that GHC knows about.
---------------------------------------------------------------------------

#include "MachDeps.h"

__interface "core" GHCziPrim 1 0 where

__export GHCziPrim

  ZLzmzgZR	-- (->)

  CCallable
  CReturnable

-- Magical assert thingy
  assert

  -- constructor tags
  tagToEnumzh
  getTagzh
  dataToTagzh

  -- I/O primitives
  RealWorld
  realWorldzh
  Statezh

  -- Concurrency primitives
  ThreadIdzh
  myThreadIdzh
  forkzh
  yieldzh
  killThreadzh
  blockAsyncExceptionszh
  unblockAsyncExceptionszh
  delayzh
  waitReadzh
  waitWritezh

  -- MVars
  MVarzh
  sameMVarzh
  newMVarzh
  takeMVarzh
  putMVarzh
  tryTakeMVarzh
  tryPutMVarzh
  isEmptyMVarzh

  -- Seq
  seq		-- Defined in MkId

  -- Parallel
  seqzh
  parzh
  parGlobalzh
  parLocalzh
  parAtzh
  parAtAbszh
  parAtRelzh
  parAtForNowzh

  -- Character Type
  Charzh 
  gtCharzh
  geCharzh
  eqCharzh
  neCharzh
  ltCharzh
  leCharzh
  ordzh
  chrzh

  -- Int Type
  Intzh
  zgzh
  zgzezh
  zezezh
  zszezh
  zlzh
  zlzezh
  zpzh
  zmzh
  ztzh
  quotIntzh
  remIntzh
  gcdIntzh
  negateIntzh
  uncheckedIShiftLzh
  uncheckedIShiftRAzh
  uncheckedIShiftRLzh
  addIntCzh
  subIntCzh
  mulIntMayOflozh

  Wordzh
  gtWordzh
  geWordzh
  eqWordzh
  neWordzh
  ltWordzh
  leWordzh
  plusWordzh
  minusWordzh
  timesWordzh
  quotWordzh
  remWordzh
  andzh
  orzh
  notzh
  xorzh
  uncheckedShiftLzh
  uncheckedShiftRLzh
  int2Wordzh
  word2Intzh

  narrow8Intzh
  narrow16Intzh
  narrow32Intzh
  narrow8Wordzh
  narrow16Wordzh
  narrow32Wordzh

#if WORD_SIZE_IN_BITS < 32
  Int32zh
  Word32zh
#endif

#if WORD_SIZE_IN_BITS < 64
  Int64zh
  Word64zh
#endif

  Addrzh
  nullAddrzh	-- Defined in MkId
  plusAddrzh
  minusAddrzh
  remAddrzh
#if (WORD_SIZE_IN_BITS == 32 || WORD_SIZE_IN_BITS == 64)
  addr2Intzh
  int2Addrzh
#endif
  gtAddrzh
  geAddrzh
  eqAddrzh
  neAddrzh
  ltAddrzh
  leAddrzh

  Floatzh
  gtFloatzh
  geFloatzh
  eqFloatzh
  neFloatzh
  ltFloatzh
  leFloatzh
  plusFloatzh
  minusFloatzh
  timesFloatzh
  divideFloatzh
  negateFloatzh
  float2Intzh
  int2Floatzh
  expFloatzh
  logFloatzh
  sqrtFloatzh
  sinFloatzh
  cosFloatzh
  tanFloatzh
  asinFloatzh
  acosFloatzh
  atanFloatzh
  sinhFloatzh
  coshFloatzh
  tanhFloatzh
  powerFloatzh
  decodeFloatzh

  Doublezh
  zgzhzh
  zgzezhzh
  zezezhzh
  zszezhzh
  zlzhzh
  zlzezhzh
  zpzhzh
  zmzhzh
  ztzhzh
  zszhzh
  negateDoublezh
  double2Intzh
  int2Doublezh
  double2Floatzh
  float2Doublezh
  expDoublezh
  logDoublezh
  sqrtDoublezh
  sinDoublezh
  cosDoublezh
  tanDoublezh
  asinDoublezh
  acosDoublezh
  atanDoublezh
  sinhDoublezh
  coshDoublezh
  tanhDoublezh
  ztztzhzh
  decodeDoublezh

-- Integer is implemented by foreign imports on .NET, so no primops

#ifndef ILX
  cmpIntegerzh
  cmpIntegerIntzh
  plusIntegerzh
  minusIntegerzh
  timesIntegerzh
  gcdIntegerzh
  quotIntegerzh
  remIntegerzh
  gcdIntegerzh
  gcdIntegerIntzh
  divExactIntegerzh
  quotRemIntegerzh
  divModIntegerzh
  integer2Intzh
  integer2Wordzh
  int2Integerzh
  word2Integerzh
#if WORD_SIZE_IN_BITS < 32
  integerToInt32zh
  integerToWord32zh
  int32ToIntegerzh
  word32ToIntegerzh
#endif  
#if WORD_SIZE_IN_BITS < 64
  int64ToIntegerzh
  word64ToIntegerzh
#endif
  andIntegerzh
  orIntegerzh
  xorIntegerzh
  complementIntegerzh
#endif

  Arrayzh
  ByteArrayzh
  MutableArrayzh
  MutableByteArrayzh
  sameMutableArrayzh
  sameMutableByteArrayzh
  newArrayzh
  newByteArrayzh
  newPinnedByteArrayzh
  byteArrayContentszh

  indexArrayzh
  indexCharArrayzh
  indexWideCharArrayzh
  indexIntArrayzh
  indexWordArrayzh
  indexAddrArrayzh
  indexFloatArrayzh
  indexDoubleArrayzh
  indexStablePtrArrayzh
  indexInt8Arrayzh
  indexInt16Arrayzh
  indexInt32Arrayzh
  indexInt64Arrayzh
  indexWord8Arrayzh
  indexWord16Arrayzh
  indexWord32Arrayzh
  indexWord64Arrayzh

  readArrayzh
  readCharArrayzh
  readWideCharArrayzh
  readIntArrayzh
  readWordArrayzh
  readAddrArrayzh
  readFloatArrayzh
  readDoubleArrayzh
  readStablePtrArrayzh
  readInt8Arrayzh
  readInt16Arrayzh
  readInt32Arrayzh
  readInt64Arrayzh
  readWord8Arrayzh
  readWord16Arrayzh
  readWord32Arrayzh
  readWord64Arrayzh

  writeArrayzh
  writeCharArrayzh
  writeWideCharArrayzh
  writeIntArrayzh
  writeWordArrayzh
  writeAddrArrayzh
  writeFloatArrayzh
  writeDoubleArrayzh
  writeStablePtrArrayzh
  writeInt8Arrayzh
  writeInt16Arrayzh
  writeInt32Arrayzh
  writeInt64Arrayzh
  writeWord8Arrayzh
  writeWord16Arrayzh
  writeWord32Arrayzh
  writeWord64Arrayzh

  indexCharOffAddrzh
  indexWideCharOffAddrzh
  indexIntOffAddrzh
  indexWordOffAddrzh
  indexAddrOffAddrzh
  indexFloatOffAddrzh
  indexDoubleOffAddrzh
  indexStablePtrOffAddrzh
  indexInt8OffAddrzh
  indexInt16OffAddrzh
  indexInt32OffAddrzh
  indexInt64OffAddrzh
  indexWord8OffAddrzh
  indexWord16OffAddrzh
  indexWord32OffAddrzh
  indexWord64OffAddrzh

  readCharOffAddrzh
  readWideCharOffAddrzh
  readIntOffAddrzh
  readWordOffAddrzh
  readAddrOffAddrzh
  readFloatOffAddrzh
  readDoubleOffAddrzh
  readStablePtrOffAddrzh
  readInt8OffAddrzh
  readInt16OffAddrzh
  readInt32OffAddrzh
  readInt64OffAddrzh
  readWord8OffAddrzh
  readWord16OffAddrzh
  readWord32OffAddrzh
  readWord64OffAddrzh

  writeCharOffAddrzh
  writeWideCharOffAddrzh
  writeIntOffAddrzh
  writeWordOffAddrzh
  writeAddrOffAddrzh
  writeForeignObjOffAddrzh
  writeFloatOffAddrzh
  writeDoubleOffAddrzh
  writeStablePtrOffAddrzh
  writeInt8OffAddrzh
  writeInt16OffAddrzh
  writeInt32OffAddrzh
  writeInt64OffAddrzh
  writeWord8OffAddrzh
  writeWord16OffAddrzh
  writeWord32OffAddrzh
  writeWord64OffAddrzh

  eqForeignObjzh
  indexCharOffForeignObjzh
  indexWideCharOffForeignObjzh
  indexIntOffForeignObjzh
  indexWordOffForeignObjzh
  indexAddrOffForeignObjzh
  indexFloatOffForeignObjzh
  indexDoubleOffForeignObjzh
  indexStablePtrOffForeignObjzh
  indexInt8OffForeignObjzh
  indexInt16OffForeignObjzh
  indexInt32OffForeignObjzh
  indexInt64OffForeignObjzh
  indexWord8OffForeignObjzh
  indexWord16OffForeignObjzh
  indexWord32OffForeignObjzh
  indexWord64OffForeignObjzh

  unsafeFreezzeArrayzh		-- Note zz in the middle
  unsafeFreezzeByteArrayzh	-- Ditto

  unsafeThawArrayzh

  sizzeofByteArrayzh		-- Ditto
  sizzeofMutableByteArrayzh	-- Ditto

  MutVarzh
  newMutVarzh
  readMutVarzh
  writeMutVarzh
  sameMutVarzh

  catchzh
  raisezh

  Weakzh
  mkWeakzh
  deRefWeakzh
  finalizzeWeakzh

  ForeignObjzh
  mkForeignObjzh
  writeForeignObjzh
  foreignObjToAddrzh
  touchzh

  StablePtrzh
  makeStablePtrzh
  deRefStablePtrzh
  eqStablePtrzh

  StableNamezh
  makeStableNamezh
  eqStableNamezh
  stableNameToIntzh

  newBCOzh
  BCOzh
  mkApUpd0zh

  unsafeCoercezh	-- unsafeCoerce# :: forall a b. a -> b
			-- It's defined in ghc/compiler/basicTypes/MkId.lhs
  addrToHValuezh
;

-- Export GHC.Err.error, so that others do not have to import PrelErr
__export GHCziErr error ;

infixr 0 seq ;

--------------------------------------------------
instance {CCallable Charzh} = zdfCCallableCharzh;
instance {CCallable Doublezh} = zdfCCallableDoublezh;
instance {CCallable Floatzh} = zdfCCallableFloatzh;
instance {CCallable Intzh} = zdfCCallableIntzh;
instance {CCallable Addrzh} = zdfCCallableAddrzh;
instance {CCallable Int64zh} = zdfCCallableInt64zh;
instance {CCallable Word64zh} = zdfCCallableWord64zh;
instance {CCallable Wordzh} = zdfCCallableWordzh;
instance {CCallable ByteArrayzh} = zdfCCallableByteArrayzh;
instance __forall s => {CCallable (MutableByteArrayzh s)} = zdfCCallableMutableByteArrayzh;
instance {CCallable ForeignObjzh} = zdfCCallableForeignObjzh;
instance __forall s => {CCallable (StablePtrzh s)} = zdfCCallableStablePtrzh;
-- CCallable and CReturnable have kind (Type AnyBox) so that
-- things like Int# can be instances of CCallable. 
1 class CCallable a :: ? ;
1 class CReturnable a :: ? ;

1 assert :: __forall a => GHCziBase.Bool -> a -> a ;

-- These guys do not really exist:
--
1 zdfCCallableCharzh :: {CCallable Charzh} ;
1 zdfCCallableDoublezh :: {CCallable Doublezh} ;
1 zdfCCallableFloatzh :: {CCallable Floatzh} ;
1 zdfCCallableIntzh :: {CCallable Intzh} ;
1 zdfCCallableAddrzh :: {CCallable Addrzh} ;
1 zdfCCallableInt64zh :: {CCallable Int64zh} ;
1 zdfCCallableWord64zh :: {CCallable Word64zh} ;
1 zdfCCallableWordzh :: {CCallable Wordzh} ;
1 zdfCCallableByteArrayzh :: {CCallable ByteArrayzh} ;
1 zdfCCallableMutableByteArrayzh :: __forall s => {CCallable (MutableByteArrayzh s)} ;
1 zdfCCallableForeignObjzh :: {CCallable ForeignObjzh} ;
1 zdfCCallableStablePtrzh :: __forall a => {CCallable (StablePtrzh a)} ;

