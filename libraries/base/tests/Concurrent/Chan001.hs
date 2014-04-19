import Debug.QuickCheck
import System.IO.Unsafe
import Control.Concurrent.Chan
import Control.Concurrent
import Control.Monad

data Action = NewChan | ReadChan | WriteChan Int | IsEmptyChan | ReturnInt Int
	    | ReturnBool Bool
  deriving (Eq,Show)


main = do 
  t <- myThreadId
  forkIO (threadDelay 1000000 >> killThread t)
	-- just in case we deadlock
  testChan

testChan :: IO ()
testChan = do
  quickCheck prop_NewIs_NewRet
  quickCheck prop_NewWriteIs_NewRet
  quickCheck prop_NewWriteRead_NewRet


prop_NewIs_NewRet = 
  [NewChan,IsEmptyChan] =^ [NewChan,ReturnBool True]

prop_NewWriteIs_NewRet n = 
  [NewChan,WriteChan n,IsEmptyChan] =^ [NewChan,WriteChan n,ReturnBool False]

prop_NewWriteRead_NewRet n = 
  [NewChan,WriteChan n,ReadChan] =^ [NewChan,ReturnInt n]


perform :: [Action] -> IO ([Bool],[Int])
perform [] = return ([],[])

perform (a:as) =
  case a of
    ReturnInt v	 -> liftM (\(b,l) -> (b,v:l)) (perform as)
    ReturnBool v -> liftM (\(b,l) -> (v:b,l)) (perform as)
    NewChan      -> newChan >>= \chan -> perform' chan as    
    _		 -> error $ "Please use NewChan as first action"


perform' :: Chan Int -> [Action] -> IO ([Bool],[Int])
perform' _ [] = return ([],[])

perform' chan (a:as) =
  case a of
    ReturnInt v	 -> liftM (\(b,l) -> (b,v:l)) (perform' chan as)
    ReturnBool v -> liftM (\(b,l) -> (v:b,l)) (perform' chan as)
    ReadChan     -> liftM2 (\v (b,l) -> (b,v:l)) (readChan chan) 
			        (perform' chan as)
    WriteChan n  -> writeChan chan n >> perform' chan as
    IsEmptyChan  -> liftM2 (\v (b,l) -> (v:b,l)) (isEmptyChan chan)
				(perform' chan as)
    _		     -> error $ "If you want to use " ++ show a 
				++ " please use the =^ operator"


actions :: Gen [Action]
actions =
  liftM (NewChan:) (actions' 0)


actions' :: Int -> Gen [Action]
actions' contents =
  oneof ([return [],
	  liftM (IsEmptyChan:) (actions' contents),
	  liftM2 (:) (liftM WriteChan arbitrary) (actions' (contents+1))]
	  ++
	  if contents==0
	     then []
	     else [liftM (ReadChan:) (actions' (contents-1))])


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

delta i (ReturnBool _:as) = delta i as

delta _ (NewChan:as) = delta 0 as

delta i (WriteChan _:as) = delta (i+1) as

delta i (ReadChan:as) = delta (if i==0
				  then error "read on empty Chan"
				  else i-1) as

delta i (IsEmptyChan:as) = delta i as
