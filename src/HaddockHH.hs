module HaddockHH(ppHHContents, ppHHIndex, ppHHProject) where

import HaddockModuleTree
import HaddockTypes
import HaddockUtil
import HsSyn hiding(Doc)
import qualified Map

import Data.Char ( toUpper )
import Data.Maybe ( fromMaybe )
import Text.PrettyPrint

ppHHContents :: FilePath -> String -> Maybe String -> [ModuleTree] -> IO ()
ppHHContents odir doctitle maybe_package tree = do
  let contentsHHFile = package++".hhc"

      html =
      	text "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML//EN\">" $$
	text "<HTML>" $$
	text "<HEAD>" $$
	text "<META name=\"GENERATOR\" content=\"Haddock\">" $$
	text "<!-- Sitemap 1.0 -->" $$
	text "</HEAD><BODY>" $$
	ppModuleTree tree $$
	text "</BODY><HTML>"
  writeFile (pathJoin [odir, contentsHHFile]) (render html)
  where
	package = fromMaybe "pkg" maybe_package
	
	ppModuleTree :: [ModuleTree] -> Doc
	ppModuleTree ts =
		text "<OBJECT type=\"text/site properties\">" $$
		text "<PARAM name=\"FrameName\" value=\"main\">" $$
		text "</OBJECT>" $$
		text "<UL>" $+$
		nest 4 (text "<LI>" <> nest 4
		                (text "<OBJECT type=\"text/sitemap\">" $$
		                 nest 4 (text "<PARAM name=\"Name\" value=\""<>text doctitle<>text "\">" $$
		                         text "<PARAM name=\"Local\" value=\"index.html\">") $$
		                 text "</OBJECT>") $+$
		        text "</LI>" $$
		        text "<UL>" $+$
		        nest 4 (fn [] ts) $+$
		        text "</UL>") $+$
		text "</UL>"

	fn :: [String] -> [ModuleTree] -> Doc
	fn ss [x]    = ppNode ss x
	fn ss (x:xs) = ppNode ss x $$ fn ss xs
        fn _  []     = error "HaddockHH.ppHHContents.fn: no module trees given"

	ppNode :: [String] -> ModuleTree -> Doc
	ppNode ss (Node s leaf _pkg _ []) =
	  ppLeaf s ss leaf
	ppNode ss (Node s leaf _pkg _ ts) =
	  ppLeaf s ss leaf $$
	  text "<UL>" $+$
	  nest 4 (fn (s:ss) ts) $+$
	  text "</UL>"

	ppLeaf s ss isleaf  =
		text "<LI>" <> nest 4
			(text "<OBJECT type=\"text/sitemap\">" $$
			 text "<PARAM name=\"Name\" value=\"" <> text s <> text "\">" $$
			 (if isleaf then text "<PARAM name=\"Local\" value=\"" <> text (moduleHtmlFile mdl) <> text "\">" else empty) $$
			 text "</OBJECT>") $+$
		text "</LI>"
		where 
			mdl = foldr (++) "" (s' : map ('.':) ss')
			(s':ss') = reverse (s:ss)
			-- reconstruct the module name
		
-------------------------------
ppHHIndex :: FilePath -> Maybe String -> [Interface] -> IO ()
ppHHIndex odir maybe_package ifaces = do
  let indexHHFile = package++".hhk"
  
      html = 
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
  writeFile (pathJoin [odir, indexHHFile]) (render html)
  where
	package = fromMaybe "pkg" maybe_package
  	
	index :: [(HsName, [Module])]
	index = Map.toAscList (foldr getIfaceIndex Map.empty ifaces)

	getIfaceIndex iface fm =
		foldl (\m (k,e) -> Map.insertWith (++) k e m) fm [(name, [mdl]) | (name, Qual mdl' _) <- Map.toAscList (iface_env iface), mdl == mdl']
		where mdl = iface_module iface
	
	ppList [] = empty
	ppList ((name,refs):mdls)  =
		text "<LI>" <> nest 4
				(text "<OBJECT type=\"text/sitemap\">" $$
				 text "<PARAM name=\"Name\" value=\"" <> text (show name) <> text "\">" $$
				 ppReference name refs $$
				 text "</OBJECT>") $+$
		text "</LI>" $$
		ppList mdls

	ppReference name [] = empty
	ppReference name (Module mdl:refs) =
		text "<PARAM name=\"Local\" value=\"" <> text (nameHtmlRef mdl name) <> text "\">" $$
		ppReference name refs


ppHHProject :: FilePath -> String -> Maybe String -> [Interface] -> [FilePath] -> IO ()
ppHHProject odir doctitle maybe_package ifaces pkg_paths = do
  let projectHHFile = package++".hhp"
      doc =
        text "[OPTIONS]" $$
        text "Compatibility=1.1 or later" $$
        text "Compiled file=" <> text package <> text ".chm" $$
        text "Contents file=" <> text package <> text ".hhc" $$
        text "Default topic=" <> text contentsHtmlFile $$
        text "Display compile progress=No" $$
        text "Index file=" <> text package <> text ".hhk" $$
        text "Title=" <> text doctitle $$
	space $$
        text "[FILES]" $$
        ppMods ifaces $$
        text contentsHtmlFile $$
        text indexHtmlFile $$
        ppIndexFiles chars $$
        ppLibFiles ("":pkg_paths)
  writeFile (pathJoin [odir, projectHHFile]) (render doc)
  where
    package = fromMaybe "pkg" maybe_package
	
    ppMods [] = empty
    ppMods (iface:ifaces) =
	let Module mdl = iface_module iface in
        text (moduleHtmlFile mdl) $$
        ppMods ifaces
		
    ppIndexFiles []     = empty
    ppIndexFiles (c:cs) =
        text (subIndexHtmlFile c) $$
        ppIndexFiles cs
        
    ppLibFiles []           = empty
    ppLibFiles (path:paths) =
        ppLibFile cssFile   $$
    	ppLibFile iconFile  $$
    	ppLibFile jsFile    $$
    	ppLibFile plusFile  $$
        ppLibFile minusFile $$
        ppLibFiles paths
        where
            toPath fname | null path = fname
	                 | otherwise = pathJoin [path, fname]
            ppLibFile fname = text (toPath fname)

    chars :: [Char]
    chars = map fst (Map.toAscList (foldr getIfaceIndex Map.empty ifaces))

    getIfaceIndex iface fm =
        Map.union (Map.fromList [(toUpper (head (show name)),()) | (name, Qual mdl' _) <- Map.toAscList (iface_env iface), mdl == mdl']) fm
	where mdl = iface_module iface
