import Debug.QuickCheck
import System.IO.Unsafe
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad


data Action = NewEmptyMVar | NewMVar Int | TakeMVar | ReadMVar | PutMVar Int
	    | SwapMVar Int | IsEmptyMVar | ReturnInt Int | ReturnBool Bool
  deriving (Eq,Show)

main = do 
  t <- myThreadId
  forkIO (threadDelay 1000000 >> killThread t)
	-- just in case we deadlock
  testMVar

testMVar :: IO ()
testMVar = do
  quickCheck prop_NewEIs_NewERet
  quickCheck prop_NewIs_NewRet
  quickCheck prop_NewTake_NewRet
  quickCheck prop_NewEPutTake_NewERet
  quickCheck prop_NewRead_NewRet
  quickCheck prop_NewSwap_New


prop_NewEIs_NewERet = 
  [NewEmptyMVar,IsEmptyMVar] =^ [NewEmptyMVar,ReturnBool True]

prop_NewIs_NewRet n = 
  [NewMVar n,IsEmptyMVar] =^ [NewMVar n,ReturnBool False]

prop_NewTake_NewRet n =
  [NewMVar n,TakeMVar] =^ [NewEmptyMVar,ReturnInt n]

prop_NewEPutTake_NewERet n = 
  [NewEmptyMVar,PutMVar n,TakeMVar] =^ 
    [NewEmptyMVar,ReturnInt n]

prop_NewRead_NewRet n = 
  [NewMVar n,ReadMVar] =^ [NewMVar n,ReturnInt n]

prop_NewSwap_New m n =
  [NewMVar m,SwapMVar n] =^ [NewMVar n]


perform :: [Action] -> IO ([Bool],[Int])
perform [] = return ([],[])

perform (a:as) =
  case a of
    ReturnInt v	 -> liftM (\(b,l) -> (b,v:l)) (perform as)
    ReturnBool v -> liftM (\(b,l) -> (v:b,l)) (perform as)
    NewEmptyMVar -> newEmptyMVar >>= \mv -> perform' mv as
    NewMVar n    -> newMVar n >>= \mv -> perform' mv as    
    _		 -> error $ "Please use NewMVar or NewEmptyMVar as first "
			    ++ "action"


perform' :: MVar Int -> [Action] -> IO ([Bool],[Int])
perform' _ [] = return ([],[])

perform' mv (a:as) =
  case a of
    ReturnInt v	 -> liftM (\(b,l) -> (b,v:l)) (perform' mv as)
    ReturnBool v -> liftM (\(b,l) -> (v:b,l)) (perform' mv as)
    TakeMVar     -> liftM2 (\v (b,l) -> (b,v:l)) (takeMVar mv) 
			        (perform' mv as)
    ReadMVar     -> liftM2 (\v (b,l) -> (b,v:l)) (readMVar mv) 
			        (perform' mv as)
    PutMVar n    -> putMVar mv n >> perform' mv as
    SwapMVar n	 -> swapMVar mv n >> perform' mv as
    IsEmptyMVar  -> liftM2 (\v (b,l) -> (v:b,l)) (isEmptyMVar mv)
				(perform' mv as)
    _		 -> error $ "If you want to use " ++ show a 
			    ++ " please use the =^ operator"


actions :: Gen [Action]
actions = do
  oneof [liftM (NewEmptyMVar:) (actions' True),
	 liftM2 (:) (liftM NewMVar arbitrary) (actions' False)] 


actions' :: Bool -> Gen [Action]
actions' empty =
  oneof ([return [],
	  liftM (IsEmptyMVar:) (actions' empty)] ++
	  if empty
	     then [liftM2 (:) (liftM PutMVar arbitrary) (actions' False)]
	     else []
	  ++
	  if empty
	     then []
	     else [liftM (TakeMVar:) (actions' True)]
	  ++
	  if empty
	     then []
	     else [liftM (ReadMVar:) (actions' False)]
	  ++
	  if empty
	     then []
	     else [liftM2 (:) (liftM SwapMVar arbitrary) (actions' False)]   )


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

delta _ (NewEmptyMVar:as) = delta True as

delta _ (NewMVar _:as) = delta False as

delta b (TakeMVar:as) = delta (if b
				  then error "take on empty MVar"
				  else True) as

delta b (ReadMVar:as) = delta (if b
				  then error "read on empty MVar"
				  else False) as

delta _ (PutMVar _:as) = delta False as

delta b (SwapMVar _:as) = delta (if b
				  then error "swap on empty MVar"
				  else False) as

delta b (IsEmptyMVar:as) = delta b as
