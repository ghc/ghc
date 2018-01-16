module DCBM where

import Fwif
import Types
import Delay
import DbParallel

{- 
  The function "delaya" is used to simulate disk accesses.
  The first argument to the function is the delay (in milliseconds).
  The second argument is the address of the disk.

  Disks are assumed to be distributed one per PE.  The function
  distributes delays evenly based on the value of the address.

    delaya :: Int -> Int -> Int
 
-}


{-
  The following function checks the returned message from a transaction, returning
  True if the transaction has committed, otherwise False.
-}

isok :: (Msgt, tree) -> Bool
isok (Ok k, t) =     True
isok (Error k, t) =  False

{- 
  The following function builds a 2-3 tree. The function takes 4
  arguments. The first argument is the first record-id the tree should
  contain and the second argument is the last record-id that the tree
  should contain, i.e. the tree should contain accound records with
  indices from "lo" to "u". "lev" is the current level in the tree. When
  the buildtree funciton is called lev schould be equal to 1. This
  parameter is used to determine if the sub-trees are to be built in
  parallel. "nrec" is the maximum number of records (account records)
  hat can fit into one leaf node (disk-block/sector). The tree is build
  in parallel in the 5 upper levels. This attempts to well distributed
  (hopefully) data amongst the processors. The creation of the tree is
  done in the following way: 1) if the number of records to process
  ("diff") is less or equal to the number of records one leaf node can
  take then a single tip-node will be created. 2) if the number of
  records to put into the tree can fit into 2 tip nodes then a binary
  tree node will be created and the records distributed equaly amongst
  its sub-trees. 3) if none of 1 or 2 then a B-tree node of order 3 (2
  keys and 3 children) will be created and the records distributed
  equally amongst its children.  In Job/prog.new/bu.tr.15.x.40 it was
  shown that creation of the tree in parallel in the 5 or 6 upper levels
  was most efficient.
-}


buildtree :: Int -> Int -> Int -> Int -> Tree

buildtree lo u  lev nrec =
	if diff <= nrec then
		res_t

	else if diff <= 2*nrec then
		lt_1 `seqt` rt_1 `seqt` res_1

	else if lev <= 5 then
		rt_2 `seqt` res_2

	else
		lt_2 `seqt` mt_2 `seqt` rt_2 `seqt` res_2

	where
		diff	= u - lo + 1

		k_1	= lo + diff `quot` 2 - 1
		lt_1	= buildtree  lo		k_1	(lev+1)	nrec
		rt_1	= buildtree  (k_1+1)	u	(lev+1)	nrec

		k1_2	= lo + diff `quot` 3 - 1
		k2_2	= u  - diff `quot` 3
		lt_2	= buildtree  lo		k1_2	(lev+1)	nrec
		mt_2	= buildtree  (k1_2+1)	k2_2	(lev+1)	nrec
		rt_2	= buildtree  (k2_2+1)	u	(lev+1)	nrec

		res_t	= Tip_Acc lo diff

		res_1	= k_1 `seqi` Node1 lt_1 k_1 rt_1
		res_2	= k1_2 `seqi` k2_2 `seqi` Node2 lt_2 k1_2 mt_2 k2_2 rt_2



{-
  The following function builds a tree containing only teller records. This tree is a	
  simple binary tree and it has only one record in each leaf node. The initial values	
  of the Teller record are a balance=0 and the Branch.ID which is chosen, "randomly",	
  to be the teller_Id modulo the tps. The function takes 3 arguments. The function	
  builds a tree containing record.ids from "lo" to "u" inclusive. 
  "tps" is needed to generate a random number as a function of the Branch.Id.
-}


build_tel_tree :: Int -> Int -> Int -> Tree
build_tel_tree lo u tps =
		if lo == u then
			Tip (Teller lo (lo `mod` tps + 1) 0 (Fill_Tel 1 2))
		else
			Node1 lt k rt
		where
			k = (u - lo) `quot` 2 + lo
			lt = build_tel_tree lo k	tps
			rt = build_tel_tree (k+1) u	tps


{-
  The following function builds a tree containing only Branch records. This tree is a
  simple binary tree and it has only one record in each leaf node. The function builds
  a tree from record.ids "lo" to "u" inclusive.
-}


build_bra_tree :: Int -> Int -> Tree
build_bra_tree lo u =
	if lo == u then
		Tip (Branch lo 0 (Fill_Bra 1 2 3))
	else
		Node1 lt k rt
	where
		k = (u - lo) `quot` 2 + lo
		lt = build_bra_tree lo k
		rt = build_bra_tree (k+1) u

{-
  The following function builds the whole database required in the DebitCredit		
  Benchmark. The function takes 2 arguments. The first "tps" is used in the scaling of	
  the relations and "nrec" is how many record that is assumed to fit into one tip	
  node. The latter one is is just passed onto other sub-functions. The function	
  creates one account tree containing key values from 1 to tps*100,000, a branch tree	
  containing key values from 1 to tps, a teller tree containing the key values from 1	
  to tps*10 and an empty hisory list. All these are created in parallel in order to	
  (in theory anyway) distribute the different relations onto different processors.	
-}


builddb :: Int -> Int -> Dbt
builddb tps nrec =
--	par acc (par tel (seq bra (seq tel (seq acc db))))
	acc `par` tel `par` (bra `seqt` db)
	where
		acc = buildtree 1 (tps*100000)	1	nrec
		bra = build_bra_tree 1  tps
		tel = build_tel_tree 1 (tps*10)	tps
		db  = Root acc bra tel []

{-
  The following function attempts to update the balance in a record with an identity	
  'key' in a tree. The funciton takes the "key", a delta (ammount -ve or +ve), and a	
  tree as arguments. If the tree is a account-tree then a disk delay is included. The	
  disk is assumed to be read and then if the transaction commits (is successful) the	
  block is written back to the disk. Since there are pointers to the old version of	
  disk block there is no need to wait until it is known if the whole DebitCredit	
  transaction commits or aborts.							
-}


replace :: Int -> Int -> Tree -> (Msgt, Tree)

replace key d nd@(Tip_Acc aid nrec) =
	if not is_error && key >= aid && key <= aid+nrec then
	     write_disk 14 (read_disk 13 (Ok key, (Tip_Acc aid nrec)))
	else
	     read_disk 13 (Error key, nd)
	where
		read_disk d cont  =  seqi (read_delay d) cont
			where read_delay n =  delaya n key 
		write_disk d cont =  seqi (write_delay d) cont
			where write_delay n = delaya n key

		is_error = (key `quot` 10) `mod`  20 == 0

replace key d (Tip e) = 
	if key == get_key e then	
		(Ok key, (Tip (new_ent e)))
	else
		(Error key, (Tip e))
	where
		get_key (Teller a b c fll) = 	a
		get_key (Branch a b fll) = 	a

		new_ent (Teller tid bid bal fll) = 	Teller tid bid (bal+d) fll
		new_ent (Branch bid bal fll) =		Branch bid (bal+d) fll
				

replace key d (Node1 lt k rt) =
	if key > k then
	    (msg_r, Node1 lt k new_rt)
	else
	    (msg_l, Node1 new_lt k rt)

	where
	 	(msg_r, new_rt) = replace key d rt
		(msg_l, new_lt) = replace key d lt


replace key d (Node2 lt k1 mt k2 rt) = 
	if key > k2 then 
	    (msg_r, Node2 lt k1 mt k2 new_rt)

	else if key > k1 then
	    (msg_m, Node2 lt k1 new_mt k2 rt)

	else
	    (msg_l, Node2 new_lt k1 mt k2 rt)

	where
	       (msg_r, new_rt) = replace key d rt
	       (msg_m, new_mt) = replace key d mt
	       (msg_l, new_lt) = replace key d lt


{-
  The following function represents one DebitCredit transacton. It takes an account
  id -- "aid", a branch id -- "bid", a teller id -- tid, a sum of money either negative
  or positive -- "delta" and a database -- "db" as arguments. The function returns a
  message and a new updated database if the transaction commits, otherwise the
  transaction failed and it returns the original database. For each of the committed
  transactions a record is inserted into the history list, where the record contains	
  the 4 first parameters to this function and a time stamp, which is currently only 
  set to 0. In the complete version of the program "ret_msg" is returned and in the
  simplified version of the program the transaction is assumed to commit and "Ok aid"	
  is returned. However, in the latter case the database will always be consistent
  whether the transaction failed or not, even if the return message is assumed to be	
  ok. (It's only the message returned that is affected by the simplification assumption.
-}


dctrans :: Int -> Int -> Int -> Int-> Transaction

dctrans aid bid tid delta db = 
	par ret_acc (par ret_bra (par ret_tel
--	(Ok aid, Root ret_acc ret_bra ret_tel ret_his )))
--	(Ok (res `seqd` acct a_result), res)))
	(Ok (acct a_result), Root ret_acc ret_bra ret_tel ret_his)))
	where
		(Root acc bra tel his) = db
--		res = ret_acc `seqt` ret_bra `seqt` ret_tel `seqt`
--		      (ret_his `seql` Root ret_acc ret_bra ret_tel ret_his)

		a_result = replace aid delta acc
		b_result = replace bid delta bra
		t_result = replace tid delta tel
		h_result = (Ok 0, His aid bid tid delta 0:his)

		p = isok a_result && isok b_result && isok t_result

		ret_acc = fwifdb p (snd a_result) acc
		ret_bra = fwifdb p (snd b_result) bra
		ret_tel = fwifdb p (snd t_result) tel
		ret_his = if p then snd h_result else his


acct (Ok aid,_) = 	aid
acct (Error aid,_) = 	aid


{-
  The following function attempts to create a list of "n" random
  DebitCredit transactions. The randomly number aid is chosen with
  respect of the size of the database and hence to the scaling factor,
  which is dependent of the "tps". The sub-function 'txs_maker' takes a
  list of random numbers and creates and returns a list of "no" number
  of 'dctrans' transactions. The function randtxs takes also the "tps
  for use in scaling of the parameters to 'dctrans'.
-}

randtxs :: Int -> Int -> [Transaction]

randtxs n tps = 
	txs_maker n (my_rnds (tps*100000))
	where
		my_rnds r = f 4364567
		  where
			f a	= abs (((a `quot` m1)*r) `quot` m1) :  f ((mult a b + 1)  `mod`  m)

			b	= 31415821
			m	= 100000000
			m1	= 10000
			b, m, m1 :: Int

			mult :: Int -> Int -> Int
			mult p q =  (((p0*q1+p1*q0)  `mod`  m1)*m1 + p0*q0) `mod` m
			  where
				p1 = p `quot` m1
				p0 = p `mod` m1
				q1 = q `quot` m1
				q0 = q `mod` m1

		txs_maker :: Int -> [Int] -> [Transaction]
		txs_maker 0 a = []
#ifdef PAR
		txs_maker no (w:ws) =  seql rest (this:rest)
#else
		txs_maker no (w:ws) =  this:rest
#endif
		  where this =
			  aid `seqi` bid `seqi` tid `seqi` delta `seqi` dctrans aid bid tid delta
			  where	aid = w `mod` (tps*100000) + 1
				tid = aid `mod` (tps*10) + 1
				bid = tid `mod` tps + 1
				delta = if aid `mod` 2 == 0 then aid `mod` 1000 else -(aid `mod` 1000)
				aid, bid, tid, delta :: Int

			rest = txs_maker (no-1) ws
			rest :: [Transaction]; this :: Transaction

{-

  The following function manages the execution of the transaction
  functions. This function takes a database and a list of transaction
  functions as arguments and returns a stream of output, which is a list
  of messages. This version is a parallel version and has been written
  by KH. There are two version of this function. One that sparks 2 tasks
  to evaluate the current txs function and one to evaluate the rest of
  the txs functions in the list the current task returns the root of the
  database form the current transaction.  However, this version seems to
  not currently work on GRIP and a simplified version has been used.

	--	par ms (par fd (m:ms))	 -- The parallel version.

-}


-- manager :: a -> [a -> (b,a)] -> [b]
manager :: Dbt -> [Transaction] -> [Msgt]

manager d (f:fs) =  par ms ml		 -- The simplified & complete version.	
	where 	fd = f d
		(m,d') = fd
		ms = manager d' fs
		ml = m `seqm` (m:ms)

manager d [] = []


{-
  The following function takes a list of messages and calculates
  something like an average the returned account.id's of the
  processed transactions. This has been done in order to use all
  of the results of the transactions and hence all of the
  transactions have to be evaluated even if lazy evaluation is the
  default.
-}


checksum :: [Msgt] -> Int
checksum (Ok k:xs) =	  (k + checksum xs) `quot` 2
checksum (Error k:xs) =   (k + checksum xs) `quot` 2
checksum [] = 0


{-
  The following function converts a string into an integer.
-}

stoi :: String -> Int
stoi st = stoi' (reverse st)
	where	stoi' [] = 0
		stoi' (x:xs) = fromEnum x - fromEnum '0' + 10 * stoi' xs
