import Debug.QuickCheck
import System.IO.Unsafe
import Control.Concurrent.QSem
import Control.Concurrent
import Control.Monad


data Action = NewQSem Int | SignalQSem | WaitQSem | GetQuantityQSem 
	    | ReturnInt Int
  deriving (Eq,Show)


main = do 
  t <- myThreadId
  forkIO (threadDelay 1000000 >> killThread t)
	-- just in case we deadlock
  testQSem

testQSem :: IO ()
testQSem = do
  quickCheck prop_NewGet_NewRet
  quickCheck prop_SignalWait
  quickCheck prop_WaitSignal


prop_NewGet_NewRet n = 
  [NewQSem n,GetQuantityQSem] =^ [NewQSem n,ReturnInt n]

prop_SignalWait n = 
  n>=0 ==> [NewQSem n,SignalQSem,WaitQSem] =^ [NewQSem n]

prop_WaitSignal n = 
  n>=1 ==> [NewQSem n,WaitQSem,SignalQSem] =^ [NewQSem n]


perform :: [Action] -> IO [Int]
perform [] = return []

perform (a:as) =
  case a of
    ReturnInt v  -> liftM (v:) (perform as)
    NewQSem n    -> newQSem n >>= \qs -> perform' qs as
    _		 -> error $ "Please use NewQSem as first action" ++ show a


perform' :: QSem -> [Action] -> IO [Int]
perform' _ [] = return []

perform' qs (a:as) =
  case a of
    ReturnInt v	     -> liftM (v:) (perform' qs as)
    SignalQSem       -> signalQSem qs >> perform' qs as
    WaitQSem         -> waitQSem qs >> perform' qs as
    GetQuantityQSem  -> liftM2 (:) (getQuantityQSem qs) (perform' qs as)
    _		     -> error $ "If you want to use " ++ show a 
			        ++ " please use the =^ operator"
   

actions :: Gen [Action]
actions = do
  i <- arbitrary
  liftM (NewQSem i:) (actions' i) 


actions' :: Int -> Gen [Action]
actions' quantity =
  oneof ([return [],
	  liftM (GetQuantityQSem:) (actions' quantity),
	  liftM (SignalQSem:) (actions' (quantity+1))] ++
	  if quantity<=0
	     then []
	     else [liftM (WaitQSem:) (actions' (quantity-1))])  


(=^) :: [Action] -> [Action] -> Property
c =^ c' =
  forAll (actions' (delta 0 c))
	 (\suff -> observe c suff == observe c' suff)
  where observe x suff = unsafePerformIO (perform (x++suff))


(^=^) :: [Action] -> [Action] -> Property
c ^=^ c' =
  forAll actions
	 (\pref -> forAll (actions' (delta 0 (pref++c)))
			  (\suff -> observe c pref suff == 
				      observe c' pref suff))
  where observe x pref suff = unsafePerformIO (perform (pref++x++suff))


delta :: Int -> [Action] -> Int
delta i [] = i

delta i (ReturnInt _:as) = delta i as

delta _ (NewQSem i:as) = delta i as

delta i (SignalQSem:as) = delta (i+1) as

delta i (WaitQSem:as) = delta (if i<=0
				  then error "wait on 'empty' QSem"
				  else i-1) as

delta i (GetQuantityQSem:as) = delta i as

