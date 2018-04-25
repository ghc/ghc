import DCBM
import Types
import DbParallel
import Fwif
import System.Environment (getArgs)

{-
  The following is the main body of the program. It attempts to create the database,	
  then create a list of random transacion functions and so run the DebitCredit		
  transactions.										
-}


--main = interact (\s -> result s ++ "\n")
main = do
    argv <- getArgs
    print (result argv)

result argv =
	if length argv < 3 then 	"Usage: tps txs disk"
	else
		"(tps,ntxs,nrec) = " ++ shows (argv,(tps,ntxs,nrec)) (
--		db `seqd` (txs `seql` show (checksum result))
		db `seqd` (txs `seql` show result)
	)

	where
--		argv :: [String]
--		argv = words args

		tps,ntxs,nrec ::  Int

		tps =  stoi (argv !! 0)
		ntxs = stoi (argv !! 1)
		nrec = stoi (argv !! 2)

		db :: Dbt
		db = builddb tps nrec

		txs :: [Transaction]
		txs = randtxs ntxs tps

		result :: [Msgt]
		result = manager db txs
