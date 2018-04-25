module Types where
data Fill =	Fill_Bra Int Int Int | Fill_Tel Int Int
		deriving Eq

data Entity =	Branch Int Int Fill | Teller Int Int Int Fill
		deriving Eq

data Tree =	Tip Entity | Tip_Acc Int Int | Node1 Tree Int Tree |
			Node2 Tree Int Tree Int Tree
		deriving Eq

data BOOL = TRUE | FALSE | UNKNOWN deriving (Show{-was:Text-})

-- The type of Database Transactions
type Transaction = Dbt -> (Msgt,Dbt)


{-
  The following is the type definition of the history recorcd (Histrt) as defined in	
  the DebitCredit benchmark. The last entity in the history record should be a time	
  stamp, but the current implementation simply sets the time stamp to the dummy value 0.
-}

data Histrt = His Int Int Int Int Int 
	deriving Eq


{- 
  The database type "Dbt" is the database as described in the
  DebitCredit benchmark. The database consists of 3 relations and a
  history of commited transactions. The first relation is the account
  relation, which is implemented as a 2-3 tree. The second relation is
  the branch relation, which is implemented as a binary tree. This has
  only one record in each of its tip-nodes. This is done since the
  records in this tree tend to be hot spots and using a big tree (many
  non-leaf nodes) will increase the efficiency of the "fwifdb"
  primitive. The third relation is the teller relation, which also is
  implemnted as a binary tree with only one record in its tip-nodes: the
  reasons are as for the branch tree. The fourth and last relation is a
  simple list of history records.
-}

data Dbt = Root Tree Tree Tree [Histrt] 
	deriving Eq


{-
  The following type definition, "Msgt", is the type of the message returned from
  evaluated transaction functions. The transactions can commit, which is represented
  as "Ok aid", or they can fail, which is represented by "Error aid".
-}

data Msgt = Ok Int | Error Int 
	deriving (Eq,Show{-was:Text-})


