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
       where flist = 	 ["PrelBase.core",
			  "PrelMaybe.core",
			  "PrelTup.core",
			  "PrelList.core", 
			  "PrelShow.core",
			  "PrelEnum.core",
			  "PrelNum.core",
			  "PrelST.core",
			  "PrelArr.core",
			  "PrelDynamic.core",
			  "PrelReal.core",
			  "PrelFloat.core",
			  "PrelRead.core",
			  "PrelIOBase.core",
			  "PrelException.core",
			  "PrelErr.core",
			  "PrelConc.core",
			  "PrelPtr.core",
			  "PrelByteArr.core",
			  "PrelPack.core",
			  "PrelBits.core",
			  "PrelWord.core",
			  "PrelInt.core",
			  "PrelCTypes.core",
			  "PrelStable.core",
			  "PrelCTypesISO.core",
			  "Monad.core",
			  "PrelStorable.core",
			  "PrelMarshalAlloc.core",
			  "PrelMarshalUtils.core",
			  "PrelMarshalArray.core",
			  "PrelCString.core",
			  "PrelMarshalError.core",
			  "PrelCError.core",
			  "PrelPosix.core",
			  "PrelHandle.core",
			  "PrelIO.core",
			  "Prelude.core",
			  "Main.core" ] 

