import Debug.QuickCheck
import System.IO.Unsafe
import Control.Concurrent.QSemN
import Control.Concurrent
import Control.Monad


main = do 
  t <- myThreadId
  forkIO (threadDelay 1000000 >> killThread t)
	-- just in case we deadlock
  testQSemN

data Action = NewQSemN Int | SignalQSemN Int | WaitQSemN Int
  deriving (Eq,Show)


testQSemN :: IO ()
testQSemN = do
  quietCheck prop_SignalWait
  quietCheck prop_WaitSignal

quietCheck = check defaultConfig{configEvery = \n args -> ""}


prop_SignalWait l m n = l+m>=n ==> 
  [NewQSemN l,SignalQSemN m,WaitQSemN n] =^ [NewQSemN (l+m-n)]

prop_WaitSignal l m n = l>=m ==> 
  [NewQSemN l,WaitQSemN m,SignalQSemN n] =^ [NewQSemN (l-m+n)]


perform :: [Action] -> IO [Int]
perform [] = return []

perform (a:as) =
  case a of
    NewQSemN n   -> newQSemN n >>= \qs -> perform' qs as
    _		 -> error $ "Please use NewQSemN as first action" ++ show a


perform' :: QSemN -> [Action] -> IO [Int]
perform' _ [] = return []

perform' qs (a:as) =
  case a of
    SignalQSemN n    -> signalQSemN qs n >> perform' qs as
    WaitQSemN n      -> waitQSemN qs n >> perform' qs as
    _		     -> error $ "If you want to use " ++ show a 
			        ++ " please use the =^ operator"
   

actions :: Gen [Action]
actions = do
  i <- arbitrary
  liftM (NewQSemN i:) (actions' i) 


actions' :: Int -> Gen [Action]
actions' quantity =
  oneof ([return [],
	  do i<- choose (0,maxBound)
	     liftM (SignalQSemN i:) (actions' (quantity+i))] ++
	  if quantity<=0
	     then []
	     else [do i<- choose (0,quantity)
		      liftM (WaitQSemN i:) (actions' (quantity-i))])  


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

delta _ (NewQSemN i:as) = delta i as

delta i (SignalQSemN n:as) = delta (i+n) as

delta i (WaitQSemN n:as) = delta (if i<n
				  then error "wait on 'empty' QSemN"
				  else i-n) as

