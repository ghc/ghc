{- A simple driver that loads, typechecks, prepares, re-typechecks, and interprets the 
    GHC standard Prelude modules and an application module called Main. 

   Note that, if compiled under GHC, this requires a very large heap to run!
-}

import Monad
import Core
import Printer
import Parser
import Lex
import ParseGlue
import Env
import Prims
import Check
import Prep
import Interp

process (senv,modules) f = 
       do putStrLn ("Processing " ++ f)
          s <- readFile f
	  case parse s 1 of
	    OkP m -> do putStrLn "Parse succeeded"
			{- writeFile (f ++ ".parsed") (show m) -}
			case checkModule senv m of
			  OkC senv' -> 
			    do putStrLn "Check succeeded"
			       let m' = prepModule senv' m
                               {- writeFile (f ++ ".prepped") (show m') -}
			       case checkModule senv m' of
                                 OkC senv'' ->
				   do putStrLn "Recheck succeeded"
                                      return (senv'',modules ++ [m'])
				 FailC s -> 
				   do putStrLn ("Recheck failed: " ++ s)
				      error "quit"
			  FailC s -> 
			    do putStrLn ("Check failed: " ++ s)
			       error "quit"
            FailP s -> do putStrLn ("Parse failed: " ++ s)
                          error "quit"

main = do (_,modules) <- foldM process (initialEnv,[]) flist
	  let result = evalProgram modules
	  putStrLn ("Result = " ++ show result)
	  putStrLn "All done"
       where flist = 	 ["PrelBase.hcr",
			  "PrelMaybe.hcr",
			  "PrelTup.hcr",
			  "PrelList.hcr", 
			  "PrelShow.hcr",
			  "PrelEnum.hcr",
			  "PrelNum.hcr",
			  "PrelST.hcr",
			  "PrelArr.hcr",
			  "PrelDynamic.hcr",
			  "PrelReal.hcr",
			  "PrelFloat.hcr",
			  "PrelRead.hcr",
			  "PrelIOBase.hcr",
			  "PrelException.hcr",
			  "PrelErr.hcr",
			  "PrelConc.hcr",
			  "PrelPtr.hcr",
			  "PrelByteArr.hcr",
			  "PrelPack.hcr",
			  "PrelBits.hcr",
			  "PrelWord.hcr",
			  "PrelInt.hcr",
			  "PrelCTypes.hcr",
			  "PrelStable.hcr",
			  "PrelCTypesISO.hcr",
			  "Monad.hcr",
			  "PrelStorable.hcr",
			  "PrelMarshalAlloc.hcr",
			  "PrelMarshalUtils.hcr",
			  "PrelMarshalArray.hcr",
			  "PrelCString.hcr",
			  "PrelMarshalError.hcr",
			  "PrelCError.hcr",
			  "PrelPosix.hcr",
			  "PrelHandle.hcr",
			  "PrelIO.hcr",
			  "Prelude.hcr",
			  "Main.hcr" ] 

