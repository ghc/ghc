-------------------------------------------------------------------------------
-- Module      :  SampleVarTest
-------------------------------------------------------------------------------

import Debug.QuickCheck
import System.IO.Unsafe
import Control.Concurrent
import Control.Concurrent.SampleVar
import Control.Monad


data Action = NewEmptySampleVar | NewSampleVar Int | EmptySampleVar 
	    | ReadSampleVar | WriteSampleVar Int | IsEmptySampleVar 
	    | ReturnInt Int | ReturnBool Bool
  deriving (Eq,Show)


main = do 
  t <- myThreadId
  forkIO (threadDelay 1000000 >> killThread t)
	-- just in case we deadlock
  testSampleVar

testSampleVar :: IO ()
testSampleVar = do
  quickCheck prop_NewEIs_NewERet
  quickCheck prop_NewIs_NewRet
  quickCheck prop_NewRead_NewRet
  quickCheck prop_NewEWriteRead_NewERet
  quickCheck prop_WriteEmpty_Empty
  quickCheck prop_WriteRead_Ret



perform :: [Action] -> IO ([Bool],[Int])
perform [] = return ([],[])

perform (a:as) =
  case a of
    ReturnInt v	      -> liftM (\(b,l) -> (b,v:l)) (perform as)
    ReturnBool v      -> liftM (\(b,l) -> (v:b,l)) (perform as)
    NewEmptySampleVar -> newEmptySampleVar >>= \sv -> perform' sv as
    NewSampleVar n    -> newSampleVar n >>= \sv -> perform' sv as    


perform' :: SampleVar Int -> [Action] -> IO ([Bool],[Int])
perform' _ [] = return ([],[])

perform' sv (a:as) =
  case a of
    ReturnInt v	      -> liftM (\(b,l) -> (b,v:l)) (perform' sv as)
    ReturnBool v      -> liftM (\(b,l) -> (v:b,l)) (perform' sv as)
    EmptySampleVar    -> emptySampleVar sv >> perform' sv as
    ReadSampleVar     -> liftM2 (\v (b,l) -> (b,v:l)) (readSampleVar sv) 
			        (perform' sv as)
    WriteSampleVar n  -> writeSampleVar sv n >> perform' sv as
    IsEmptySampleVar  -> liftM2 (\v (b,l) -> (v:b,l)) (isEmptySampleVar sv)
				(perform' sv as)


actions :: Gen [Action]
actions = do
  oneof [liftM (NewEmptySampleVar:) (actions' True),
	 liftM2 (:) (liftM NewSampleVar arbitrary) (actions' False)] 


actions' :: Bool -> Gen [Action]
actions' empty =
  oneof ([return [],
	  liftM (IsEmptySampleVar:) (actions' empty),
	  liftM (EmptySampleVar:) (actions' True),
	  liftM2 (:) (liftM WriteSampleVar arbitrary) (actions' False)] ++
	  if empty
	     then []
	     else [liftM (ReadSampleVar:) (actions' True)])  


(=^) :: [Action] -> [Action] -> Property
c =^ c' =
  forAll (actions' (delta True c))
	 (\suff -> observe c suff == observe c' suff)
  where observe x suff = unsafePerformIO (perform (x++suff))


(^=^) :: [Action] -> [Action] -> Property
c ^=^ c' =
  forAll actions
	 (\pref -> forAll (actions' (delta True (pref++c)))
			  (\suff -> observe c pref suff == 
				      observe c' pref suff))
  where observe x pref suff = unsafePerformIO (perform (pref++x++suff))


delta :: Bool -> [Action] -> Bool
delta b [] = b

delta b (ReturnInt _:as) = delta b as

delta b (ReturnBool _:as) = delta b as

delta _ (NewEmptySampleVar:as) = delta True as

delta _ (NewSampleVar _:as) = delta False as

delta _ (EmptySampleVar:as) = delta True as

delta b (ReadSampleVar:as) = delta (if b
				       then error "read on empty SampleVar"
				       else True) as 
delta _ (WriteSampleVar _:as) = delta False as

delta b (IsEmptySampleVar:as) = delta b as


prop_NewEIs_NewERet = 
  [NewEmptySampleVar,IsEmptySampleVar] =^ [NewEmptySampleVar,ReturnBool True]

prop_NewIs_NewRet n = 
  [NewSampleVar n,IsEmptySampleVar] =^ [NewSampleVar n,ReturnBool False]

prop_NewRead_NewRet n =
  [NewSampleVar n,ReadSampleVar] =^ [NewEmptySampleVar,ReturnInt n]

prop_NewEWriteRead_NewERet n = 
  [NewEmptySampleVar,WriteSampleVar n,ReadSampleVar] =^ 
    [NewEmptySampleVar,ReturnInt n]

prop_WriteEmpty_Empty n = 
  [WriteSampleVar n,EmptySampleVar] ^=^ [EmptySampleVar]

prop_WriteRead_Ret n = 
  [WriteSampleVar n,ReadSampleVar] ^=^ [EmptySampleVar,ReturnInt n]
