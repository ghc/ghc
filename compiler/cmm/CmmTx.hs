
module CmmTx where

data ChangeFlag = NoChange | SomeChange

type Tx a    = a -> TxRes a
data TxRes a = TxRes ChangeFlag a

seqTx :: Tx a -> Tx a -> Tx a
iterateTx :: Tx a -> Tx a
runTx :: Tx a -> a -> a

noTx, aTx :: a -> TxRes a
noTx x = TxRes NoChange   x
aTx  x = TxRes SomeChange x

replaceTx :: a -> TxRes b -> TxRes a
replaceTx a (TxRes change _) = TxRes change a

txVal :: TxRes a -> a
txVal (TxRes _ a) = a

txHasChanged :: TxRes a -> Bool
txHasChanged (TxRes NoChange   _) = False
txHasChanged (TxRes SomeChange _) = True

plusTx :: (a -> b -> c) -> TxRes a -> TxRes b -> TxRes c
plusTx f (TxRes c1 a) (TxRes c2 b) = TxRes (c1 `orChange` c2) (f a b)

mapTx :: Tx a -> Tx [a]
mapTx _ []     = noTx []
mapTx f (x:xs) = plusTx (:) (f x) (mapTx f xs)

runTx f = txVal . f

seqTx f1 f2 a =
    let TxRes c1 a1 = f1 a
        TxRes c2 a2 = f2 a1
    in  TxRes (c1 `orChange` c2) a2

iterateTx f a 
  = case f a of
	TxRes NoChange   a' -> TxRes NoChange a'
	TxRes SomeChange a' -> let TxRes _ a'' = iterateTx f a'
			     in TxRes SomeChange a''

orChange :: ChangeFlag -> ChangeFlag -> ChangeFlag
orChange NoChange   c = c
orChange SomeChange _ = SomeChange



instance Functor TxRes where
  fmap f (TxRes ch a) = TxRes ch (f a)

instance Monad TxRes where
    return = TxRes NoChange
    (TxRes NoChange a) >>= k = k a
    (TxRes SomeChange a) >>= k = let (TxRes _ a') = k a in TxRes SomeChange a'
