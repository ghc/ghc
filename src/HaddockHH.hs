module HaddockHH(ppHHContents, ppHHIndex) where

import HsSyn hiding(Doc)

#if __GLASGOW_HASKELL__ < 503
import Pretty
import FiniteMap
#else
import Text.PrettyPrint
import Data.FiniteMap
#endif

import HaddockModuleTree
import HaddockUtil
import HaddockTypes

contentsHHFile, indexHHFile :: String
contentsHHFile = "index.hhc"
indexHHFile = "index.hhk"

ppHHContents :: FilePath -> [Module] -> IO ()
ppHHContents odir mods = do
  let tree = mkModuleTree (zip mods (repeat Nothing)) --TODO: packages
      html =
      	text "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML//EN\">" $$
	text "<HTML>" $$
	text "<HEAD>" $$
	text "<META name=\"GENERATOR\" content=\"Haddock\">" $$
	text "<!-- Sitemap 1.0 -->" $$
	text "</HEAD><BODY>" $$
	ppModuleTree tree $$
	text "</BODY><HTML>"
  writeFile (odir ++ pathSeparator:contentsHHFile) (render html)
  where
	ppModuleTree :: [ModuleTree] -> Doc
	ppModuleTree ts =
		text "<OBJECT type=\"text/site properties\">" $$
		text "<PARAM name=\"FrameName\" value=\"main\">" $$
		text "</OBJECT>" $$
		text "<UL>" $+$
		nest 4 (fn [] ts) $+$
		text "</UL>"
  	
	fn :: [String] -> [ModuleTree] -> Doc
	fn ss [x]    = ppNode ss x
	fn ss (x:xs) = ppNode ss x $$ fn ss xs
        fn _  []     = error "HaddockHH.ppHHContents.fn: no module trees given"

	ppNode :: [String] -> ModuleTree -> Doc
	ppNode ss (Node s leaf _pkg []) =
	  ppLeaf s ss leaf
	ppNode ss (Node s leaf _pkg ts) =
	  ppLeaf s ss leaf $$
	  text "<UL>" $+$
	  nest 4 (fn (s:ss) ts) $+$
	  text "</UL>"

	ppLeaf s ss isleaf  =
		text "<LI>" <> nest 4
			(text "<OBJECT type=\"text/sitemap\">" $$
			 text "<PARAM name=\"Name\" value=\"" <> text s <> text "\">" $$
			 (if isleaf then text "<PARAM name=\"Local\" value=\"" <> text (moduleHtmlFile "" mdl) <> text "\">" else empty) $$
			 text "</OBJECT>") $+$
		text "</LI>"
		where 
			mdl = foldr (++) "" (s' : map ('.':) ss')
			(s':ss') = reverse (s:ss)
			-- reconstruct the module name
		
-------------------------------
ppHHIndex :: FilePath -> [(Module,Interface)] -> IO ()
ppHHIndex odir ifaces = do
  let html = 
      	text "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML//EN\">" $$
	text "<HTML>" $$
	text "<HEAD>" $$
	text "<META name=\"GENERATOR\" content=\"Haddock\">" $$
	text "<!-- Sitemap 1.0 -->" $$
	text "</HEAD><BODY>" $$
	text "<UL>" $+$
	nest 4 (ppList index) $+$
	text "</UL>" $$
	text "</BODY><HTML>"
  writeFile (odir ++ pathSeparator:indexHHFile) (render html)
  where	
	index :: [(HsName, Module)]
	index = fmToList full_index

	iface_indices = map getIfaceIndex ifaces
	full_index = foldr1 plusFM iface_indices

	getIfaceIndex (mdl,iface) = listToFM
	    [ (name, mdl) | (name, Qual mdl' _) <- fmToList (iface_env iface), mdl == mdl']
	
	ppList [] = empty
	ppList ((name,Module mdl):mdls)  =
		text "<LI>" <> nest 4
				(text "<OBJECT type=\"text/sitemap\">" $$
				 text "<PARAM name=\"Name\" value=\"" <> text (show name) <> text "\">" $$
				 text "<PARAM name=\"Local\" value=\"" <> text (moduleHtmlFile "" mdl) <> char '#' <> text (show name) <> text "\">" $$
				 text "</OBJECT>") $+$
		text "</LI>" $$
		ppList mdls
