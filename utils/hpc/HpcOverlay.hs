module HpcOverlay where

import HpcFlags
import HpcParser

overlay_options 
        = srcDirOpt
        . hpcDirOpt
        . outputOpt

overlay_plugin = Plugin { name = "overlay"
	      	       , usage = "[OPTION] .. <OVERLAY_FILE> [<OVERLAY_FILE> [...]]" 
		       , options = overlay_options 
		       , summary = "Generate a .tix file from an overlay file"
		       , implementation = overlay_main
		       , init_flags = default_flags
		       , final_flags = default_final_flags
		       }


overlay_main flags [] = hpcError overlay_plugin $ "no overlay file specified" 
overlay_main flags files = do
  print ("HERE", files)
  result <- hpcParser (head files)
  print result
  return ()
  
  
