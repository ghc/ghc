{- This module really should be auto-generated from the master primops.txt file. 
   It is roughly correct (but may be slightly incomplete) wrt/ GHC5.02. -}

module Prims where

import Core
import Env
import Check

initialEnv :: Menv
initialEnv = efromlist [(primMname,primEnv),
		     ("PrelErr",errorEnv)]

primEnv :: Envs
primEnv = Envs {tcenv_=efromlist primTcs,
		tsenv_=eempty,
		cenv_=efromlist primDcs,
		venv_=efromlist primVals}

errorEnv :: Envs
errorEnv = Envs {tcenv_=eempty,
		 tsenv_=eempty,
		 cenv_=eempty,
		 venv_=efromlist errorVals}

{- Components of static environment -}

primTcs :: [(Tcon,Kind)]
primTcs = 
	map (\ ((m,tc),k) -> (tc,k))
	([(tcArrow,ktArrow),
	 (tcAddrzh,ktAddrzh),
	 (tcCharzh,ktCharzh),
	 (tcDoublezh,ktDoublezh),
	 (tcFloatzh,ktFloatzh),
	 (tcIntzh,ktIntzh),
	 (tcInt32zh,ktInt32zh),
	 (tcInt64zh,ktInt64zh),
	 (tcWordzh,ktWordzh),
	 (tcWord32zh,ktWord32zh),
	 (tcWord64zh,ktWord64zh),
	 (tcRealWorld, ktRealWorld),
	 (tcStatezh, ktStatezh),
	 (tcArrayzh,ktArrayzh),
	 (tcByteArrayzh,ktByteArrayzh),
	 (tcMutableArrayzh,ktMutableArrayzh),
	 (tcMutableByteArrayzh,ktMutableByteArrayzh),
	 (tcMutVarzh,ktMutVarzh),
	 (tcMVarzh,ktMVarzh),
	 (tcWeakzh,ktWeakzh),
	 (tcForeignObjzh, ktForeignObjzh),
	 (tcStablePtrzh, ktStablePtrzh),
	 (tcThreadIdzh, ktThreadIdzh),
	 (tcZCTCCallable, ktZCTCCallable),
	 (tcZCTCReturnable, ktZCTCReturnable)]
       ++ [(tcUtuple n, ktUtuple n) | n <- [1..maxUtuple]])


primDcs :: [(Dcon,Ty)]
primDcs = map (\ ((m,c),t) -> (c,t))
	      [(dcUtuple n, dcUtupleTy n) | n <- [1..maxUtuple]]

primVals :: [(Var,Ty)]
primVals = 
	opsAddrzh ++
	opsCharzh ++
	opsDoublezh ++
	opsFloatzh ++
	opsIntzh ++
	opsInt32zh ++
	opsInt64zh ++
	opsIntegerzh ++
	opsWordzh ++
	opsWord32zh ++
	opsWord64zh ++
	opsSized ++
	opsArray ++
	opsMutVarzh ++
	opsState ++
	opsExn ++
	opsMVar ++
	opsWeak ++
	opsForeignObjzh ++
        opsStablePtrzh ++
	opsConc ++
	opsMisc


dcUtuples :: [(Qual Dcon,Ty)]
dcUtuples = map ( \n -> (dcUtuple n, typ n)) [1..100]
     where typ n = foldr ( \tv t -> Tforall (tv,Kopen) t)
				(foldr ( \tv t -> tArrow (Tvar tv) t)
					     (tUtuple (map Tvar tvs)) tvs) tvs
		      where tvs = map ( \i -> ("a" ++ (show i))) [1..n]


{- Addrzh -}

tcAddrzh = (primMname,"Addrzh")
tAddrzh = Tcon tcAddrzh
ktAddrzh = Kunlifted

opsAddrzh = [
 ("gtAddrzh",tcompare tAddrzh),
 ("geAddrzh",tcompare tAddrzh), 
 ("eqAddrzh",tcompare tAddrzh),  
 ("neAddrzh",tcompare tAddrzh),
 ("ltAddrzh",tcompare tAddrzh),  
 ("leAddrzh",tcompare tAddrzh),
 ("nullAddrzh", tAddrzh),
 ("plusAddrzh", tArrow tAddrzh (tArrow tIntzh tAddrzh)),
 ("minusAddrzh", tArrow tAddrzh (tArrow tAddrzh tIntzh)),
 ("remAddrzh", tArrow tAddrzh (tArrow tIntzh tIntzh))]

{- Charzh -}

tcCharzh = (primMname,"Charzh")
tCharzh = Tcon tcCharzh
ktCharzh = Kunlifted

opsCharzh = [
 ("gtCharzh", tcompare tCharzh),
 ("geCharzh", tcompare tCharzh),
 ("eqCharzh", tcompare tCharzh),
 ("neCharzh", tcompare tCharzh),
 ("ltCharzh", tcompare tCharzh),
 ("leCharzh", tcompare tCharzh),
 ("ordzh",    tArrow tCharzh tIntzh)]


{- Doublezh -}

tcDoublezh = (primMname, "Doublezh")
tDoublezh = Tcon tcDoublezh
ktDoublezh = Kunlifted

opsDoublezh = [
 ("zgzhzh", tcompare tDoublezh),
 ("zgzezhzh", tcompare tDoublezh),
 ("zezezhzh", tcompare tDoublezh),
 ("zszezhzh", tcompare tDoublezh),
 ("zlzhzh", tcompare tDoublezh),
 ("zlzezhzh", tcompare tDoublezh),
 ("zpzhzh", tdyadic tDoublezh),
 ("zmzhzh", tdyadic tDoublezh),
 ("ztzhzh", tdyadic tDoublezh),
 ("zszhzh", tdyadic tDoublezh),
 ("negateDoublezh", tmonadic tDoublezh),
 ("double2Intzh", tArrow tDoublezh tIntzh),
 ("double2Floatzh", tArrow tDoublezh tFloatzh),
 ("expDoublezh", tmonadic tDoublezh),
 ("logDoublezh", tmonadic tDoublezh),
 ("sqrtDoublezh", tmonadic tDoublezh),
 ("sinDoublezh", tmonadic tDoublezh),
 ("cosDoublezh", tmonadic tDoublezh),
 ("tanDoublezh", tmonadic tDoublezh),
 ("asinDoublezh", tmonadic tDoublezh),
 ("acosDoublezh", tmonadic tDoublezh),
 ("atanDoublezh", tmonadic tDoublezh),
 ("sinhDoublezh", tmonadic tDoublezh),
 ("coshDoublezh", tmonadic tDoublezh),
 ("tanhDoublezh", tmonadic tDoublezh),
 ("ztztzhzh", tdyadic tDoublezh),
 ("decodeDoublezh", tArrow tDoublezh (tUtuple[tIntzh,tIntzh,tByteArrayzh]))]


{- Floatzh -}

tcFloatzh = (primMname, "Floatzh")
tFloatzh = Tcon tcFloatzh
ktFloatzh = Kunlifted

opsFloatzh = [
 ("gtFloatzh", tcompare tFloatzh),
 ("geFloatzh", tcompare tFloatzh),
 ("eqFloatzh", tcompare tFloatzh),
 ("neFloatzh", tcompare tFloatzh),
 ("ltFloatzh", tcompare tFloatzh),
 ("leFloatzh", tcompare tFloatzh),
 ("plusFloatzh", tdyadic tFloatzh),
 ("minusFloatzh", tdyadic tFloatzh),
 ("timesFloatzh", tdyadic tFloatzh),
 ("divideFloatzh", tdyadic tFloatzh),
 ("negateFloatzh", tmonadic tFloatzh),
 ("float2Intzh", tArrow tFloatzh tIntzh),
 ("expFloatzh", tmonadic tFloatzh),
 ("logFloatzh", tmonadic tFloatzh),
 ("sqrtFloatzh", tmonadic tFloatzh),
 ("sinFloatzh", tmonadic tFloatzh),
 ("cosFloatzh", tmonadic tFloatzh),
 ("tanFloatzh", tmonadic tFloatzh),
 ("asinFloatzh", tmonadic tFloatzh),
 ("acosFloatzh", tmonadic tFloatzh),
 ("atanFloatzh", tmonadic tFloatzh),
 ("sinhFloatzh", tmonadic tFloatzh),
 ("coshFloatzh", tmonadic tFloatzh),
 ("tanhFloatzh", tmonadic tFloatzh),
 ("powerFloatzh", tdyadic tFloatzh),
 ("float2Doublezh", tArrow tFloatzh tDoublezh),
 ("decodeFloatzh", tArrow tFloatzh (tUtuple[tIntzh,tIntzh,tByteArrayzh]))]


{- Intzh -}

tcIntzh = (primMname,"Intzh")
tIntzh = Tcon tcIntzh
ktIntzh = Kunlifted

opsIntzh = [
 ("zpzh", tdyadic tIntzh),
 ("zmzh", tdyadic tIntzh),
 ("ztzh", tdyadic tIntzh),
 ("quotIntzh", tdyadic tIntzh),
 ("remIntzh", tdyadic tIntzh),
 ("gcdIntzh", tdyadic tIntzh),
 ("negateIntzh", tmonadic tIntzh),
 ("addIntCzh", tArrow tIntzh (tArrow tIntzh (tUtuple [tIntzh, tIntzh]))),
 ("subIntCzh", tArrow tIntzh (tArrow tIntzh (tUtuple [tIntzh, tIntzh]))),
 ("mulIntCzh", tArrow tIntzh (tArrow tIntzh (tUtuple [tIntzh, tIntzh]))),
 ("zgzh", tcompare tIntzh),
 ("zgzezh", tcompare tIntzh),
 ("zezezh", tcompare tIntzh),
 ("zszezh", tcompare tIntzh),
 ("zlzh", tcompare tIntzh),
 ("zlzezh", tcompare tIntzh),
 ("chrzh", tArrow tIntzh tCharzh),
 ("int2Wordzh", tArrow tIntzh tWordzh),
 ("int2Floatzh", tArrow tIntzh tFloatzh),
 ("int2Doublezh", tArrow tIntzh tDoublezh),
 ("intToInt32zh", tArrow tIntzh tInt32zh),
 ("int2Integerzh", tArrow tIntzh tIntegerzhRes),
 ("iShiftLzh", tdyadic tIntzh),
 ("iShiftRAzh", tdyadic tIntzh),
 ("iShiftRLh", tdyadic tIntzh)]


{- Int32zh -}

tcInt32zh = (primMname,"Int32zh")
tInt32zh = Tcon tcInt32zh
ktInt32zh = Kunlifted

opsInt32zh = [
 ("int32ToIntzh", tArrow tInt32zh tIntzh),
 ("int32ToIntegerzh", tArrow tInt32zh tIntegerzhRes)]


{- Int64zh -}

tcInt64zh = (primMname,"Int64zh")
tInt64zh = Tcon tcInt64zh
ktInt64zh = Kunlifted

opsInt64zh = [
 ("int64ToIntegerzh", tArrow tInt64zh tIntegerzhRes)]

{- Integerzh -}

-- not actuallly a primitive type
tIntegerzhRes = tUtuple [tIntzh, tByteArrayzh]
tIntegerzhTo t = tArrow tIntzh (tArrow tByteArrayzh t)
tdyadicIntegerzh = tIntegerzhTo (tIntegerzhTo tIntegerzhRes)

opsIntegerzh = [
 ("plusIntegerzh", tdyadicIntegerzh),
 ("minusIntegerzh", tdyadicIntegerzh),
 ("timesIntegerzh", tdyadicIntegerzh),
 ("gcdIntegerzh", tdyadicIntegerzh),
 ("gcdIntegerIntzh", tIntegerzhTo (tArrow tIntzh tIntzh)),
 ("divExactIntegerzh", tdyadicIntegerzh),
 ("quotIntegerzh", tdyadicIntegerzh),
 ("remIntegerzh", tdyadicIntegerzh),
 ("cmpIntegerzh", tIntegerzhTo (tIntegerzhTo tIntzh)),
 ("cmpIntegerIntzh", tIntegerzhTo (tArrow tIntzh tIntzh)),
 ("quotRemIntegerzh", tIntegerzhTo (tIntegerzhTo (tUtuple [tIntzh,tByteArrayzh,tIntzh,tByteArrayzh]))),
 ("divModIntegerzh", tIntegerzhTo (tIntegerzhTo (tUtuple [tIntzh,tByteArrayzh,tIntzh,tByteArrayzh]))),
 ("integer2Intzh", tIntegerzhTo tIntzh),
 ("integer2Wordzh", tIntegerzhTo tWordzh),
 ("integerToInt32zh", tIntegerzhTo tInt32zh),
 ("integerToWord32zh", tIntegerzhTo tWord32zh),
 ("integerToInt64zh", tIntegerzhTo tInt64zh),
 ("integerToWord64zh", tIntegerzhTo tWord64zh),
 ("andIntegerzh", tdyadicIntegerzh),
 ("orIntegerzh", tdyadicIntegerzh),
 ("xorIntegerzh", tdyadicIntegerzh),
 ("complementIntegerzh", tIntegerzhTo tIntegerzhRes)]



{- Wordzh -}

tcWordzh = (primMname,"Wordzh")
tWordzh = Tcon tcWordzh
ktWordzh = Kunlifted

opsWordzh = [
 ("plusWordzh", tdyadic tWordzh),
 ("minusWordzh", tdyadic tWordzh),
 ("timesWordzh", tdyadic tWordzh),
 ("quotWordzh",   tdyadic tWordzh),
 ("remWordzh",   tdyadic tWordzh),
 ("andzh",    tdyadic tWordzh),
 ("orzh",    tdyadic tWordzh),
 ("xorzh",    tdyadic tWordzh),
 ("notzh",    tmonadic tWordzh),
 ("shiftLzh", tArrow tWordzh (tArrow tIntzh tWordzh)),
 ("shiftRLzh", tArrow tWordzh (tArrow tIntzh tWordzh)),
 ("word2Intzh", tArrow tWordzh tIntzh),
 ("wordToWord32zh", tArrow tWordzh tWord32zh),
 ("word2Integerzh", tArrow tWordzh tIntegerzhRes),
 ("gtWordzh", tcompare tWordzh),
 ("geWordzh", tcompare tWordzh),
 ("eqWordzh", tcompare tWordzh),
 ("neWordzh", tcompare tWordzh),
 ("ltWordzh", tcompare tWordzh),
 ("leWordzh", tcompare tWordzh)]

{- Word32zh -}

tcWord32zh = (primMname,"Word32zh")
tWord32zh = Tcon tcWord32zh
ktWord32zh = Kunlifted

opsWord32zh = [
 ("word32ToWordzh", tArrow tWord32zh tWordzh),
 ("word32ToIntegerzh", tArrow tWord32zh tIntegerzhRes)]

{- Word64zh -}

tcWord64zh = (primMname,"Word64zh")
tWord64zh = Tcon tcWord64zh
ktWord64zh = Kunlifted

opsWord64zh = [
 ("word64ToIntegerzh", tArrow tWord64zh tIntegerzhRes)]

{- Explicitly sized Intzh and Wordzh -}

opsSized = [
 ("narrow8Intzh", tmonadic tIntzh),
 ("narrow16Intzh", tmonadic tIntzh),
 ("narrow32Intzh", tmonadic tIntzh),
 ("narrow8Wordzh", tmonadic tWordzh),
 ("narrow16Wordzh", tmonadic tWordzh),
 ("narrow32Wordzh", tmonadic tWordzh)]

{- Arrays -}

tcArrayzh = (primMname,"Arrayzh")
tArrayzh t = Tapp (Tcon tcArrayzh) t
ktArrayzh = Karrow Klifted Kunlifted

tcByteArrayzh = (primMname,"ByteArrayzh")
tByteArrayzh = Tcon tcByteArrayzh
ktByteArrayzh = Kunlifted

tcMutableArrayzh = (primMname,"MutableArrayzh")
tMutableArrayzh s t = Tapp (Tapp (Tcon tcMutableArrayzh) s) t
ktMutableArrayzh = Karrow Klifted (Karrow Klifted Kunlifted)

tcMutableByteArrayzh = (primMname,"MutableByteArrayzh")
tMutableByteArrayzh s = Tapp (Tcon tcMutableByteArrayzh) s
ktMutableByteArrayzh = Karrow Klifted Kunlifted

opsArray = [
 ("newArrayzh", Tforall ("a",Klifted) 
		       (Tforall ("s",Klifted)
                                (tArrow tIntzh 
                                       (tArrow (Tvar "a")
                                               (tArrow (tStatezh (Tvar "s"))
                                                       (tUtuple [tStatezh (Tvar "s"),tMutableArrayzh (Tvar "s") (Tvar "a")])))))),
 ("newByteArrayzh", Tforall ("s",Klifted)
                           (tArrow tIntzh 
                                   (tArrow (tStatezh (Tvar "s"))
                                           (tUtuple [tStatezh (Tvar "s"),tMutableByteArrayzh (Tvar "s")])))),
 ("newPinnedByteArrayzh", Tforall ("s",Klifted)
                           (tArrow tIntzh 
                                   (tArrow (tStatezh (Tvar "s"))
                                           (tUtuple [tStatezh (Tvar "s"),tMutableByteArrayzh (Tvar "s")])))),
 ("byteArrayContentszh", tArrow tByteArrayzh tAddrzh),
 ("indexCharArrayzh", tArrow tByteArrayzh (tArrow tIntzh tCharzh)),
 ("indexWideCharArrayzh", tArrow tByteArrayzh (tArrow tIntzh tCharzh)),
 ("indexIntArrayzh", tArrow tByteArrayzh (tArrow tIntzh tIntzh)),
 ("indexWordArrayzh", tArrow tByteArrayzh (tArrow tIntzh tWordzh)),
 ("indexAddrArrayzh", tArrow tByteArrayzh (tArrow tIntzh tAddrzh)),
 ("indexFloatArrayzh", tArrow tByteArrayzh (tArrow tIntzh tFloatzh)),
 ("indexDoubleArrayzh", tArrow tByteArrayzh (tArrow tIntzh tDoublezh)),
 ("indexStablePtrArrayzh", Tforall ("a",Klifted) (tArrow tByteArrayzh (tArrow tIntzh (tStablePtrzh (Tvar "a"))))),
 ("indexInt8Arrayzh", tArrow tByteArrayzh (tArrow tIntzh tIntzh)),
 ("indexInt16Arrayzh", tArrow tByteArrayzh (tArrow tIntzh tIntzh)),
 ("indexInt32Arrayzh", tArrow tByteArrayzh (tArrow tIntzh tInt32zh)),
 ("indexInt64Arrayzh", tArrow tByteArrayzh (tArrow tIntzh tInt64zh)),
 ("indexWord8Arrayzh", tArrow tByteArrayzh (tArrow tIntzh tWordzh)),
 ("indexWord16Arrayzh", tArrow tByteArrayzh (tArrow tIntzh tWordzh)),
 ("indexWord32Arrayzh", tArrow tByteArrayzh (tArrow tIntzh tWord32zh)),
 ("indexWord64Arrayzh", tArrow tByteArrayzh (tArrow tIntzh tWord64zh)),
 ("readCharArrayzh", tReadMutableByteArrayzh tCharzh),
 ("readWideCharArrayzh", tReadMutableByteArrayzh tCharzh),
 ("readIntArrayzh", tReadMutableByteArrayzh tIntzh),
 ("readWordArrayzh", tReadMutableByteArrayzh tWordzh),
 ("readAddrArrayzh", tReadMutableByteArrayzh tAddrzh),
 ("readFloatArrayzh", tReadMutableByteArrayzh tFloatzh),
 ("readDoubleArrayzh", tReadMutableByteArrayzh tDoublezh),
 ("readStablePtrArrayzh", Tforall ("s",Klifted)
				 (Tforall ("a",Klifted)
					  (tArrow (tMutableByteArrayzh (Tvar "s"))
				                  (tArrow tIntzh
				                         (tArrow (tStatezh (Tvar "s"))
				                                 (tUtuple [tStatezh (Tvar "s"),tStablePtrzh (Tvar "a")])))))),
 ("readInt8Arrayzh", tReadMutableByteArrayzh tIntzh),
 ("readInt16Arrayzh", tReadMutableByteArrayzh tIntzh),
 ("readInt32Arrayzh", tReadMutableByteArrayzh tInt32zh),
 ("readInt64Arrayzh", tReadMutableByteArrayzh tInt64zh),
 ("readWord8Arrayzh", tReadMutableByteArrayzh tWordzh),
 ("readWord16Arrayzh", tReadMutableByteArrayzh tWordzh),
 ("readWord32Arrayzh", tReadMutableByteArrayzh tWord32zh),
 ("readWord64Arrayzh", tReadMutableByteArrayzh tWord64zh),

 ("writeCharArrayzh", tWriteMutableByteArrayzh tCharzh),
 ("writeWideCharArrayzh", tWriteMutableByteArrayzh tCharzh),
 ("writeIntArrayzh", tWriteMutableByteArrayzh tIntzh),
 ("writeWordArrayzh", tWriteMutableByteArrayzh tWordzh),
 ("writeAddrArrayzh", tWriteMutableByteArrayzh tAddrzh),
 ("writeFloatArrayzh", tWriteMutableByteArrayzh tFloatzh),
 ("writeDoubleArrayzh", tWriteMutableByteArrayzh tDoublezh),
 ("writeStablePtrArrayzh", Tforall ("s",Klifted)
				  (Tforall ("a",Klifted)
				           (tArrow (tMutableByteArrayzh (Tvar "s"))
				                   (tArrow tIntzh
				                          (tArrow (tStablePtrzh (Tvar "a"))
			  		                          (tArrow (tStatezh (Tvar "s"))
				                                          (tStatezh (Tvar "s")))))))),
 ("writeInt8Arrayzh", tWriteMutableByteArrayzh tIntzh),
 ("writeInt16Arrayzh", tWriteMutableByteArrayzh tIntzh),
 ("writeInt32Arrayzh", tWriteMutableByteArrayzh tIntzh),
 ("writeInt64Arrayzh", tWriteMutableByteArrayzh tInt64zh),
 ("writeWord8Arrayzh", tWriteMutableByteArrayzh tWordzh),
 ("writeWord16Arrayzh", tWriteMutableByteArrayzh tWordzh),
 ("writeWord32Arrayzh", tWriteMutableByteArrayzh tWord32zh),
 ("writeWord64Arrayzh", tWriteMutableByteArrayzh tWord64zh),

 ("indexCharOffAddrzh", tArrow tAddrzh (tArrow tIntzh tCharzh)),
 ("indexWideCharOffAddrzh", tArrow tAddrzh (tArrow tIntzh tCharzh)),
 ("indexIntOffAddrzh", tArrow tAddrzh (tArrow tIntzh tIntzh)),
 ("indexWordOffAddrzh", tArrow tAddrzh (tArrow tIntzh tWordzh)),
 ("indexAddrOffAddrzh", tArrow tAddrzh (tArrow tIntzh tAddrzh)),
 ("indexFloatOffAddrzh", tArrow tAddrzh (tArrow tIntzh tFloatzh)),
 ("indexDoubleOffAddrzh", tArrow tAddrzh (tArrow tIntzh tDoublezh)),
 ("indexStablePtrOffAddrzh", Tforall ("a",Klifted) (tArrow tAddrzh (tArrow tIntzh (tStablePtrzh (Tvar "a"))))),
 ("indexInt8OffAddrzh", tArrow tAddrzh (tArrow tIntzh tIntzh)),
 ("indexInt16OffAddrzh", tArrow tAddrzh (tArrow tIntzh tIntzh)),
 ("indexInt32OffAddrzh", tArrow tAddrzh (tArrow tIntzh tInt32zh)),
 ("indexInt64OffAddrzh", tArrow tAddrzh (tArrow tIntzh tInt64zh)),
 ("indexWord8OffAddrzh", tArrow tAddrzh (tArrow tIntzh tWordzh)),
 ("indexWord16OffAddrzh", tArrow tAddrzh (tArrow tIntzh tWordzh)),
 ("indexWord32ffAddrzh", tArrow tAddrzh (tArrow tIntzh tWord32zh)),
 ("indexWord64OffAddrzh", tArrow tAddrzh (tArrow tIntzh tWord64zh)),

 ("indexCharOffForeignObjzh", tArrow tForeignObjzh (tArrow tIntzh tCharzh)),
 ("indexWideCharOffForeignObjzh", tArrow tForeignObjzh (tArrow tIntzh tCharzh)),
 ("indexIntOffForeignObjzh", tArrow tForeignObjzh (tArrow tIntzh tIntzh)),
 ("indexWordOffForeignObjzh", tArrow tForeignObjzh (tArrow tIntzh tWordzh)),
 ("indexAddrOffForeignObjzh", tArrow tForeignObjzh (tArrow tIntzh tAddrzh)),
 ("indexFloatOffForeignObjzh", tArrow tForeignObjzh (tArrow tIntzh tFloatzh)),
 ("indexDoubleOffForeignObjzh", tArrow tForeignObjzh (tArrow tIntzh tDoublezh)),
 ("indexStablePtrOffForeignObjzh", Tforall ("a",Klifted) (tArrow tForeignObjzh (tArrow tIntzh (tStablePtrzh (Tvar "a"))))),
 ("indexInt8OffForeignObjzh", tArrow tForeignObjzh (tArrow tIntzh tIntzh)),
 ("indexInt16OffForeignObjzh", tArrow tForeignObjzh (tArrow tIntzh tIntzh)),
 ("indexInt32OffForeignObjzh", tArrow tForeignObjzh (tArrow tIntzh tInt32zh)),
 ("indexInt64OffForeignObjzh", tArrow tForeignObjzh (tArrow tIntzh tInt64zh)),
 ("indexWord8OffForeignObjzh", tArrow tForeignObjzh (tArrow tIntzh tWordzh)),
 ("indexWord16OffForeignObjzh", tArrow tForeignObjzh (tArrow tIntzh tWordzh)),
 ("indexWord32ffForeignObjzh", tArrow tForeignObjzh (tArrow tIntzh tWord32zh)),
 ("indexWord64OffForeignObjzh", tArrow tForeignObjzh (tArrow tIntzh tWord64zh)),

 ("readCharOffAddrzh", tReadOffAddrzh tCharzh),
 ("readWideCharOffAddrzh", tReadOffAddrzh tCharzh),
 ("readIntOffAddrzh", tReadOffAddrzh tIntzh),
 ("readWordOffAddrzh", tReadOffAddrzh tWordzh),
 ("readAddrOffAddrzh", tReadOffAddrzh tAddrzh),
 ("readFloatOffAddrzh", tReadOffAddrzh tFloatzh),
 ("readDoubleOffAddrzh", tReadOffAddrzh tDoublezh),
 ("readStablePtrOffAddrzh", Tforall ("s",Klifted) 
				(Tforall ("a",Klifted)
			             (tArrow tAddrzh
			                     (tArrow tIntzh
                        			    (tArrow (tStatezh (Tvar "s"))
			                                    (tUtuple [tStatezh (Tvar "s"),tStablePtrzh (Tvar "a")])))))),
 ("readInt8OffAddrzh", tReadOffAddrzh tIntzh),
 ("readInt16OffAddrzh", tReadOffAddrzh tIntzh),
 ("readInt32OffAddrzh", tReadOffAddrzh tInt32zh),
 ("readInt64OffAddrzh", tReadOffAddrzh tInt64zh),
 ("readWord8OffAddrzh", tReadOffAddrzh tWordzh),
 ("readWord16OffAddrzh", tReadOffAddrzh tWordzh),
 ("readWord32OffAddrzh", tReadOffAddrzh tWord32zh),
 ("readWord64OffAddrzh", tReadOffAddrzh tWord64zh),

 ("writeCharOffAddrzh", tWriteOffAddrzh tCharzh),
 ("writeWideCharOffAddrzh", tWriteOffAddrzh tCharzh),
 ("writeIntOffAddrzh", tWriteOffAddrzh tIntzh),
 ("writeWordOffAddrzh", tWriteOffAddrzh tWordzh),
 ("writeAddrOffAddrzh", tWriteOffAddrzh tAddrzh),
 ("writeFloatOffAddrzh", tWriteOffAddrzh tFloatzh),
 ("writeDoubleOffAddrzh", tWriteOffAddrzh tDoublezh),
 ("writeStablePtrOffAddrzh", Tforall ("a",Klifted) (tWriteOffAddrzh (tStablePtrzh (Tvar "a")))),
 ("writeInt8OffAddrzh", tWriteOffAddrzh tIntzh),
 ("writeInt16OffAddrzh", tWriteOffAddrzh tIntzh),
 ("writeInt32OffAddrzh", tWriteOffAddrzh tInt32zh),
 ("writeInt64OffAddrzh", tWriteOffAddrzh tInt64zh),
 ("writeWord8OffAddrzh", tWriteOffAddrzh tWordzh),
 ("writeWord16OffAddrzh", tWriteOffAddrzh tWordzh),
 ("writeWord32OffAddrzh", tWriteOffAddrzh tWord32zh),
 ("writeWord64OffAddrzh", tWriteOffAddrzh tWord64zh),
 
 ("sameMutableArrayzh", Tforall ("s",Klifted)
                               (Tforall ("a",Klifted)
                                        (tArrow (tMutableArrayzh (Tvar "s") (Tvar "a"))
                                                (tArrow (tMutableArrayzh (Tvar "s") (Tvar "a"))
                                                        tBool)))),
 ("sameMutableByteArrayzh", Tforall ("s",Klifted)
                                   (tArrow (tMutableByteArrayzh (Tvar "s"))
                                           (tArrow (tMutableByteArrayzh (Tvar "s"))
                                                   tBool))),
 ("readArrayzh",Tforall ("s",Klifted)
                        (Tforall ("a",Klifted)
                                 (tArrow (tMutableArrayzh (Tvar "s") (Tvar "a"))
			                 (tArrow tIntzh
					         (tArrow (tStatezh (Tvar "s"))
                                                         (tUtuple[tStatezh (Tvar "s"), Tvar "a"])))))),
 ("writeArrayzh",Tforall ("s",Klifted)
                         (Tforall ("a",Klifted)
                                  (tArrow (tMutableArrayzh (Tvar "s") (Tvar "a"))
			                  (tArrow tIntzh
				  	          (tArrow (Tvar "a")
					                  (tArrow (tStatezh (Tvar "s"))
                                                                  (tStatezh (Tvar "s")))))))),
 ("indexArrayzh", Tforall ("a",Klifted)
                          (tArrow (tArrayzh (Tvar "a"))
                                  (tArrow tIntzh
                                          (tUtuple[Tvar "a"])))),
 ("unsafeFreezzeArrayzh",Tforall ("s",Klifted)
                                (Tforall ("a",Klifted)
                                         (tArrow (tMutableArrayzh (Tvar "s") (Tvar "a"))
						 (tArrow (tStatezh (Tvar "s"))
                                                         (tUtuple[tStatezh (Tvar "s"),tArrayzh (Tvar "a")]))))),
 ("unsafeFreezzeByteArrayzh",Tforall ("s",Klifted)
                                    (tArrow (tMutableByteArrayzh (Tvar "s"))
				     	    (tArrow (tStatezh (Tvar "s"))
                                                    (tUtuple[tStatezh (Tvar "s"),tByteArrayzh])))),
 ("unsafeThawArrayzh",Tforall ("a",Klifted)
                             (Tforall ("s",Klifted)
                                      (tArrow (tArrayzh (Tvar "a"))
					      (tArrow (tStatezh (Tvar "s"))
                                                      (tUtuple[tStatezh (Tvar "s"),tMutableArrayzh (Tvar "s") (Tvar "a")]))))),
 ("sizzeofByteArrayzh", tArrow tByteArrayzh tIntzh), 
 ("sizzeofMutableByteArrayzh", Tforall ("s",Klifted) (tArrow (tMutableByteArrayzh (Tvar "s")) tIntzh))]
 where
 tReadMutableByteArrayzh t = 
     Tforall ("s",Klifted)
             (tArrow (tMutableByteArrayzh (Tvar "s"))
                     (tArrow tIntzh
                            (tArrow (tStatezh (Tvar "s"))
                                    (tUtuple [tStatezh (Tvar "s"),t]))))

 tWriteMutableByteArrayzh t = 
     Tforall ("s",Klifted)
             (tArrow (tMutableByteArrayzh (Tvar "s"))
                     (tArrow tIntzh
                            (tArrow t
                                    (tArrow (tStatezh (Tvar "s"))
                                            (tStatezh (Tvar "s"))))))

 tReadOffAddrzh t = 
     Tforall ("s",Klifted)
             (tArrow tAddrzh
                     (tArrow tIntzh
                            (tArrow (tStatezh (Tvar "s"))
                                    (tUtuple [tStatezh (Tvar "s"),t]))))


 tWriteOffAddrzh t = 
     Tforall ("s",Klifted)
             (tArrow tAddrzh
                     (tArrow tIntzh
                            (tArrow t
                                    (tArrow (tStatezh (Tvar "s"))
                                            (tStatezh (Tvar "s"))))))

{- MutVars -}

tcMutVarzh = (primMname,"MutVarzh")
tMutVarzh s t = Tapp (Tapp (Tcon tcMutVarzh) s) t
ktMutVarzh = Karrow Klifted (Karrow Klifted Kunlifted)

opsMutVarzh = [
 ("newMutVarzh", Tforall ("a",Klifted)    
                    (Tforall ("s",Klifted)
                        (tArrow (Tvar "a") (tArrow (tStatezh (Tvar "s"))
						   (tUtuple [tStatezh (Tvar "s"),
						             tMutVarzh (Tvar "s") (Tvar "a")]))))),
 ("readMutVarzh", Tforall ("s",Klifted)
		    (Tforall ("a",Klifted)
                        (tArrow (tMutVarzh (Tvar "s")(Tvar "a"))
				(tArrow (tStatezh (Tvar "s"))
                                        (tUtuple [tStatezh (Tvar "s"), Tvar "a"]))))),
 ("writeMutVarzh", Tforall ("s",Klifted)
		    (Tforall ("a",Klifted)
                        (tArrow (tMutVarzh (Tvar "s") (Tvar "a"))
				(tArrow (Tvar "a")
                                        (tArrow (tStatezh (Tvar "s"))
                                                (tStatezh (Tvar "s"))))))),
 ("sameMutVarzh", Tforall ("s",Klifted)
	   	    (Tforall ("a",Klifted)
                        (tArrow (tMutVarzh (Tvar "s") (Tvar "a"))
				(tArrow (tMutVarzh (Tvar "s") (Tvar "a"))
				        tBool))))]

{- Real world and state. -}

tcRealWorld = (primMname,"RealWorld")
tRealWorld = Tcon tcRealWorld
ktRealWorld = Klifted

tcStatezh = (primMname, "Statezh")
tStatezh t = Tapp (Tcon tcStatezh) t
ktStatezh = Karrow Klifted Kunlifted

tRWS = tStatezh tRealWorld

opsState = [
  ("realWorldzh", tRWS)]

{- Exceptions -}

-- no primitive type
opsExn = [
 ("catchzh", 
	let t' = tArrow tRWS (tUtuple [tRWS, Tvar "a"]) in
	Tforall ("a",Klifted)
		(Tforall ("b",Klifted)
			 (tArrow t'
				 (tArrow (tArrow (Tvar "b") t') 
                                         t')))),
  ("raisezh", Tforall ("a",Klifted)
		      (Tforall ("b",Klifted)
			       (tArrow (Tvar "a") (Tvar "b")))),
  ("blockAsyncExceptionszh", Tforall ("a",Klifted)			
                                    (tArrow (tArrow tRWS (tUtuple[tRWS,Tvar "a"]))
					    (tArrow tRWS (tUtuple[tRWS,Tvar "a"])))),
  ("unblockAsyncExceptionszh", Tforall ("a",Klifted)			
                                    (tArrow (tArrow tRWS (tUtuple[tRWS,Tvar "a"]))
					    (tArrow tRWS (tUtuple[tRWS,Tvar "a"]))))]

{- Mvars -} 

tcMVarzh = (primMname, "MVarzh")
tMVarzh s t = Tapp (Tapp (Tcon tcMVarzh) s) t
ktMVarzh = Karrow Klifted (Karrow Klifted Kunlifted)

opsMVar = [
 ("newMVarzh", Tforall ("s",Klifted)
		      (Tforall ("a",Klifted)
                               (tArrow (tStatezh (Tvar "s"))
				       (tUtuple[tStatezh (Tvar "s"),tMVarzh (Tvar "s") (Tvar "a")])))),
 ("takeMVarzh", Tforall ("s",Klifted)
		      (Tforall ("a",Klifted)
			       (tArrow (tMVarzh (Tvar "s") (Tvar "a"))
				       (tArrow (tStatezh (Tvar "s"))
					       (tUtuple[tStatezh (Tvar "s"),Tvar "a"]))))),
 ("tryTakeMVarzh", Tforall ("s",Klifted)
		      (Tforall ("a",Klifted)
			       (tArrow (tMVarzh (Tvar "s") (Tvar "a"))
				       (tArrow (tStatezh (Tvar "s"))
					       (tUtuple[tStatezh (Tvar "s"),tIntzh,Tvar "a"]))))),
 ("putMVarzh", Tforall ("s",Klifted)
		      (Tforall ("a",Klifted)
			       (tArrow (tMVarzh (Tvar "s") (Tvar "a"))
				       (tArrow (Tvar "a")
				               (tArrow (tStatezh (Tvar "s"))
					               (tStatezh (Tvar "s"))))))),
 ("tryPutMVarzh", Tforall ("s",Klifted)
		      (Tforall ("a",Klifted)
			       (tArrow (tMVarzh (Tvar "s") (Tvar "a"))
				       (tArrow (Tvar "a")
				               (tArrow (tStatezh (Tvar "s"))
					               (tUtuple [tStatezh (Tvar "s"), tIntzh])))))),
 ("sameMVarzh", Tforall ("s",Klifted)
		      (Tforall ("a",Klifted)
			       (tArrow (tMVarzh (Tvar "s") (Tvar "a"))
				       (tArrow (tMVarzh (Tvar "s") (Tvar "a"))
					       tBool)))),
 ("isEmptyMVarzh", Tforall ("s",Klifted)
		      (Tforall ("a",Klifted)
			       (tArrow (tMVarzh (Tvar "s") (Tvar "a"))
				       (tArrow (tStatezh (Tvar "s"))
					       (tUtuple[tStatezh (Tvar "s"),tIntzh])))))]


{- Weak Objects -}

tcWeakzh = (primMname, "Weakzh")
tWeakzh t = Tapp (Tcon tcWeakzh) t
ktWeakzh = Karrow Klifted Kunlifted

opsWeak = [
  ("mkWeakzh", Tforall ("o",Kopen)
                      (Tforall ("b",Klifted)
			       (Tforall ("c",Klifted)
					(tArrow (Tvar "o")
					        (tArrow (Tvar "b")
							(tArrow (Tvar "c")
			                                        (tArrow tRWS (tUtuple[tRWS, tWeakzh (Tvar "b")])))))))),
  ("deRefWeakzh", Tforall ("a",Klifted)
			 (tArrow (tWeakzh (Tvar "a"))
			         (tArrow tRWS (tUtuple[tRWS, tIntzh, Tvar "a"])))),
  ("finalizeWeakzh", Tforall ("a",Klifted)
			    (tArrow (tWeakzh (Tvar "a"))
                                    (tArrow tRWS
					    (tUtuple[tRWS,tIntzh,
						     tArrow tRWS (tUtuple[tRWS, tUnit])]))))]


{- Foreign Objects -}

tcForeignObjzh = (primMname, "ForeignObjzh")
tForeignObjzh = Tcon tcForeignObjzh
ktForeignObjzh = Kunlifted

opsForeignObjzh = [
 ("mkForeignObjzh", tArrow tAddrzh
		           (tArrow tRWS (tUtuple [tRWS,tForeignObjzh]))),
 ("writeForeignObjzh", Tforall ("s",Klifted) 
		               (tArrow tForeignObjzh
			               (tArrow tAddrzh
				               (tArrow (tStatezh (Tvar "s")) (tStatezh (Tvar "s")))))),
 ("foreignObjToAddrzh", tArrow tForeignObjzh tAddrzh),
 ("touchzh", Tforall ("o",Kopen)
		     (tArrow (Tvar "o")
			     (tArrow tRWS tRWS)))]


{- Stable Pointers (but not names) -}

tcStablePtrzh = (primMname, "StablePtrzh")
tStablePtrzh t = Tapp (Tcon tcStablePtrzh) t
ktStablePtrzh = Karrow Klifted Kunlifted

opsStablePtrzh = [
  ("makeStablePtrzh", Tforall ("a",Klifted) 
			     (tArrow (Tvar "a")
				     (tArrow tRWS (tUtuple[tRWS,tStablePtrzh (Tvar "a")])))),
  ("deRefStablePtrzh", Tforall ("a",Klifted)
			      (tArrow (tStablePtrzh (Tvar "a"))
				      (tArrow tRWS (tUtuple[tRWS,Tvar "a"])))),
  ("eqStablePtrzh", Tforall ("a",Klifted)
			   (tArrow (tStablePtrzh (Tvar "a"))
                                   (tArrow (tStablePtrzh (Tvar "a")) tIntzh)))]

{- Concurrency  operations -}

tcThreadIdzh = (primMname,"ThreadIdzh")
tThreadIdzh = Tcon tcThreadIdzh
ktThreadIdzh = Kunlifted

opsConc = [
  ("seqzh", Tforall ("a",Klifted)
		    (tArrow (Tvar "a") tIntzh)),
  ("parzh", Tforall ("a",Klifted)
		    (tArrow (Tvar "a") tIntzh)),
  ("delayzh", Tforall ("s",Klifted)
		     (tArrow tIntzh (tArrow (tStatezh (Tvar "s")) (tStatezh (Tvar "s"))))),
  ("waitReadzh", Tforall ("s",Klifted)
		     (tArrow tIntzh (tArrow (tStatezh (Tvar "s")) (tStatezh (Tvar "s"))))),
  ("waitWritezh", Tforall ("s",Klifted)
		     (tArrow tIntzh (tArrow (tStatezh (Tvar "s")) (tStatezh (Tvar "s"))))),
  ("forkzh", Tforall ("a",Klifted)
		    (tArrow (Tvar "a") 
			    (tArrow tRWS (tUtuple[tRWS,tThreadIdzh])))),
  ("killThreadzh", Tforall ("a",Klifted)
		          (tArrow tThreadIdzh
			          (tArrow (Tvar "a")
                                          (tArrow tRWS tRWS)))),
  ("yieldzh", tArrow tRWS tRWS),
  ("myThreadIdzh", tArrow tRWS (tUtuple[tRWS, tThreadIdzh]))]

{- Miscellaneous operations -}

opsMisc =  [
  ("dataToTagzh", Tforall ("a",Klifted) 
			  (tArrow (Tvar "a") tIntzh)),
  ("tagToEnumzh", Tforall ("a",Klifted)
                          (tArrow tIntzh (Tvar "a"))),
  ("unsafeCoercezh", Tforall ("a",Kopen) 
                             (Tforall ("b",Kopen) 
                                      (tArrow (Tvar "a") (Tvar "b")))) -- maybe unneeded
  ]

{- CCallable and CReturnable.
   We just define the type constructors for the dictionaries
   corresponding to these pseudo-classes. -}

tcZCTCCallable = (primMname,"ZCTCCallable")
ktZCTCCallable = Karrow Kopen Klifted  -- ??
tcZCTCReturnable = (primMname,"ZCTCReturnable")
ktZCTCReturnable = Karrow Kopen Klifted  -- ??

{- Non-primitive, but mentioned in the types of primitives. -}

tcUnit = ("PrelBase","Unit")
tUnit = Tcon tcUnit
ktUnit = Klifted
tcBool = ("PrelBase","Bool")
tBool = Tcon tcBool
ktBool = Klifted

{- Properly defined in PrelError, but needed in many modules before that. -}
errorVals = [
 ("error", Tforall ("a",Kopen) (tArrow tString (Tvar "a"))),
 ("irrefutPatError", Tforall ("a",Kopen) (tArrow tString (Tvar "a"))),
 ("patError", Tforall ("a",Kopen) (tArrow tString (Tvar "a")))]
  
tcChar = ("PrelBase","Char")
tChar = Tcon tcChar
ktChar = Klifted
tcList = ("PrelBase","ZMZN")
tList t = Tapp (Tcon tcList) t
ktList = Karrow Klifted Klifted
tString = tList tChar

{- Utilities for building types -}
tmonadic t = tArrow t t
tdyadic t = tArrow t (tArrow t t)
tcompare t = tArrow t (tArrow t tBool)

