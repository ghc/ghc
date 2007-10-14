-- (c) 2007 Andy Gill

-- Main driver for Hpc
import Trace.Hpc.Tix
import HpcFlags
import System.Environment
import System.Exit
import System.Console.GetOpt

import HpcReport
import HpcMarkup
import HpcCombine
import HpcShowTix
import HpcDraft
import HpcOverlay

helpList :: IO ()
helpList =
     putStrLn $ 
           "Usage: hpc COMMAND ...\n\n" ++ 
    	   section "Commands" help ++
	   section "Reporting Coverage" reporting ++
	   section "Processing Coverage files" processing ++
	   section "Coverage Overlays" overlays ++
	   section "Others" other ++
	   ""
  where 
    help       = ["help"]
    reporting  = ["report","markup"]
    overlays   = ["overlay","draft"]
    processing = ["sum","combine","map"]
    other     = [ name hook
    	        | hook <- hooks
		, name hook `notElem` 
		     (concat [help,reporting,processing,overlays])
		]

section :: String -> [String] -> String
section msg []   = ""
section msg cmds = msg ++ ":\n" 
        ++ unlines [ take 14 ("  " ++ cmd ++ repeat ' ') ++ summary hook
	   	   | cmd <- cmds
		   , hook <- hooks 
		   , name hook == cmd
		   ]

dispatch :: [String] -> IO ()
dispatch [] = do
	     helpList
	     exitWith ExitSuccess
dispatch (txt:args) = do
     case lookup txt hooks' of
       Just plugin -> parse plugin args
       _ -> parse help_plugin (txt:args)
  where
     parse plugin args =
              case getOpt Permute (options plugin []) args of
                (_,_,errs) | not (null errs)
                     -> do putStrLn "hpc failed:"
                	   sequence [ putStr ("  " ++ err)
          		 	    | err <- errs 
          			    ]
          	           putStrLn $ "\n"
                           command_usage plugin
          		   exitFailure
	        (o,ns,_) -> do
			 let flags = final_flags plugin 
			           $ foldr (.) id o 
			     	   $ init_flags plugin
			 implementation plugin flags ns
main = do 
 args <- getArgs
 dispatch args

------------------------------------------------------------------------------

hooks = [ help_plugin
        , report_plugin 
	, markup_plugin
	, sum_plugin
	, combine_plugin
	, map_plugin
	, showtix_plugin
	, overlay_plugin
	, draft_plugin
	, version_plugin
        ]

hooks' = [ (name hook,hook) | hook <- hooks ]

------------------------------------------------------------------------------

help_plugin = Plugin { name = "help"
		   , usage = "[<HPC_COMMAND>]"
		   , summary = "Display help for hpc or a single command"
		   , options = help_options
		   , implementation = help_main
		   , init_flags = default_flags
		   , final_flags = default_final_flags
		   }

help_main flags [] = do
	    helpList
	    exitWith ExitSuccess	    
help_main flags (sub_txt:_) = do
    case lookup sub_txt hooks' of
      Nothing -> do
	  putStrLn $ "no such hpc command : " ++ sub_txt
	  exitFailure
      Just plugin' -> do
	  command_usage plugin'
	  exitWith ExitSuccess

help_options   = id

------------------------------------------------------------------------------

version_plugin = Plugin { name = "version"
		   , usage = ""
		   , summary = "Display version for hpc"
		   , options = id
		   , implementation = version_main
		   , init_flags = default_flags
		   , final_flags = default_final_flags
		   }

version_main _ _ = putStrLn $ "hpc tools, version 0.6"


------------------------------------------------------------------------------
