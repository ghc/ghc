module HaddockHH2(ppHH2Contents, ppHH2Index, ppHH2Files, ppHH2Collection) where

import HsSyn hiding(Doc)

#if __GLASGOW_HASKELL__ < 503
import Pretty
import FiniteMap
#else
import Text.PrettyPrint
import Data.FiniteMap
import Data.List
import Data.Char
#endif

import HaddockModuleTree
import HaddockUtil
import HaddockTypes

ppHH2Contents :: FilePath -> String -> [(Module,Interface)] -> IO ()
ppHH2Contents odir package ifaces = do
  let 	
	contentsHH2File = package++".HxT"

	tree = mkModuleTree (map (\(mod,_) -> (mod,Nothing)) ifaces) --TODO: packages
	doc  =
		text "<?xml version=\"1.0\"?>" $$
		text "<!DOCTYPE HelpTOC SYSTEM \"ms-help://hx/resources/HelpTOC.DTD\">" $$
		text "<HelpTOC DTDVersion=\"1.0\">" $$
		nest 4 (ppModuleTree [] tree) $$
		text "</HelpTOC>"
  writeFile (odir ++ pathSeparator:contentsHH2File) (render doc)
  where
	ppModuleTree :: [String] -> [ModuleTree] -> Doc
	ppModuleTree ss [x]    = ppNode ss x
	ppModuleTree ss (x:xs) = ppNode ss x $$ ppModuleTree ss xs
	ppModuleTree _  []     = error "HaddockHH2.ppHH2Contents.ppModuleTree: no module trees given"

	ppNode :: [String] -> ModuleTree -> Doc
	ppNode ss (Node s leaf _pkg []) =
	  text "<HelpTOCNode"  <+> ppAttributes leaf (s:ss) <> text "/>"
	ppNode ss (Node s leaf _pkg ts) =
	  text "<HelpTOCNode" <+> ppAttributes leaf (s:ss) <> text ">" $$
	  nest 4 (ppModuleTree (s:ss) ts) $+$
	  text "</HelpTOCNode>"
			
	ppAttributes :: Bool -> [String] -> Doc
	ppAttributes isleaf ss = hsep [ppId,ppTitle,ppUrl]
	  where
	    mdl = foldr (++) "" (s' : map ('.':) ss')
	    (s':ss') = reverse ss
	                -- reconstruct the module name
	    
	    ppId = text "Id=" <> doubleQuotes (text mdl)
	    
	    ppTitle = text "Title=" <> doubleQuotes (text (head ss))
	    
	    ppUrl | isleaf    = text " Url=" <> doubleQuotes (text (moduleHtmlFile "" mdl))
	          | otherwise = empty

-----------------------------------------------------------------------------------

ppHH2Index :: FilePath -> String -> [(Module,Interface)] -> IO ()
ppHH2Index odir package ifaces = do
  let 
	indexKHH2File     = package++"K.HxK"
	indexNHH2File     = package++"N.HxK"
	docK = 
		text "<?xml version=\"1.0\"?>" $$
		text "<!DOCTYPE HelpIndex SYSTEM \"ms-help://hx/resources/HelpIndex.DTD\">" $$
		text "<HelpIndex DTDVersion=\"1.0\" Name=\"K\">" $$
		nest 4 (ppList index) $+$
		text "</HelpIndex>"  
	docN = 
		text "<?xml version=\"1.0\"?>" $$
		text "<!DOCTYPE HelpIndex SYSTEM \"ms-help://hx/resources/HelpIndex.DTD\">" $$
		text "<HelpIndex DTDVersion=\"1.0\" Name=\"NamedURLIndex\">" $$
		text "<Keyword Term=\"HomePage\">" $$
		nest 4 (text "<Jump Url=\"index.html\"/>") $$
		text "</Keyword>" $$
		text "</HelpIndex>"
  writeFile (odir ++ pathSeparator:indexKHH2File) (render docK)
  writeFile (odir ++ pathSeparator:indexNHH2File) (render docN)
  where	
	index :: [(HsName, [Module])]
	index = fmToList (foldr getIfaceIndex emptyFM ifaces)

	getIfaceIndex (mdl,iface) fm =
	    addListToFM_C (++) fm [(name, [mdl]) | (name, Qual mdl' _) <- fmToList (iface_env iface), mdl == mdl']
	
	ppList [] = empty
	ppList ((name,mdls):vs)  =
		text "<Keyword Term=\"" <> text (escapeStr (show name)) <> text "\">" $$
		nest 4 (vcat (map (ppJump name) mdls)) $$
		text "</Keyword>" $$
		ppList vs

	ppJump name (Module mdl) = text "<Jump Url=\"" <> text (nameHtmlRef fp mdl name) <> text "\"/>"
	  where
	    fp = case lookupFM html_xrefs (Module mdl) of
	      Nothing  -> ""
	      Just fp0 -> fp0


-----------------------------------------------------------------------------------

ppHH2Files :: FilePath -> String -> [(Module,Interface)] -> IO ()
ppHH2Files odir package ifaces = do
  let filesHH2File = package++".HxF"
      doc =
        text "<?xml version=\"1.0\"?>" $$
        text "<!DOCTYPE HelpFileList SYSTEM \"ms-help://hx/resources/HelpFileList.DTD\">" $$
        text "<HelpFileList DTDVersion=\"1.0\">" $$
        nest 4 (ppMods ifaces $$
                text "<File Url=\"index.html\"/>" $$
                text "<File Url=\"doc-index.html\"/>" $$
                ppIndexFiles chars $$
                text "<File Url=\""<>text iconFile<>text "\"/>" $$
                text "<File Url=\""<>text cssFile<>text "\"/>") $$
        text "</HelpFileList>"
  writeFile (odir ++ pathSeparator:filesHH2File) (render doc)
  where
    ppMods [] = empty
    ppMods ((Module mdl,_):ifaces) =
		text "<File Url=\"" <> text (moduleHtmlFile "" mdl) <> text "\"/>" $$
		ppMods ifaces
		
    ppIndexFiles []     = empty
    ppIndexFiles (c:cs) =
        text "<File Url=\"doc-index-" <> char c <> text ".html\"/>" $$
        ppIndexFiles cs
	
    chars :: [Char]
    chars = keysFM (foldr getIfaceIndex emptyFM ifaces)

    getIfaceIndex (mdl,iface) fm =
        addListToFM fm [(toUpper (head (show name)),()) | (name, Qual mdl' _) <- fmToList (iface_env iface), mdl == mdl']

-----------------------------------------------------------------------------------

ppHH2Collection :: FilePath -> String -> [(Module,Interface)] -> IO ()
ppHH2Collection odir package ifaces = do
  let 
	collectionHH2File = package++".HxC"
	
	doc =
		text "<?xml version=\"1.0\"?>" $$
		text "<!DOCTYPE HelpCollection SYSTEM \"ms-help://hx/resources/HelpCollection.DTD\">" $$
		text "<HelpCollection DTDVersion=\"1.0\" LangId=\"1033\" Title=\"" <> text package <> text "\">" $$
		nest 4 (text "<CompilerOptions CreateFullTextIndex=\"Yes\">" $$
		        nest 4 (text "<IncludeFile File=\"" <> text package <> text ".HxF\"/>") $$
		        text "</CompilerOptions>" $$
		        text "<TOCDef File=\"" <> text package <> text ".HxT\"/>" $$
		        text "<KeywordIndexDef File=\"" <> text package <> text "K.HxK\"/>" $$
		        text "<KeywordIndexDef File=\"" <> text package <> text "N.HxK\"/>" $$
		        text "<ItemMoniker Name=\"!DefaultToc\" ProgId=\"HxDs.HxHierarchy\" InitData=\"\"/>" $$
		        text "<ItemMoniker Name=\"!DefaultFullTextSearch\" ProgId=\"HxDs.HxFullTextSearch\" InitData=\"\"/>" $$
		        text "<ItemMoniker Name=\"!DefaultAssociativeIndex\" ProgId=\"HxDs.HxIndex\" InitData=\"A\"/>" $$
		        text "<ItemMoniker Name=\"!DefaultKeywordIndex\" ProgId=\"HxDs.HxIndex\" InitData=\"K\"/>" $$
		        text "<ItemMoniker Name=\"!DefaultNamedUrlIndex\" ProgId=\"HxDs.HxIndex\" InitData=\"NamedURLIndex\"/>" $$
		        text "<ItemMoniker Name=\"!SampleInfo\" ProgId=\"HxDs.HxSampleCollection\" InitData=\"\"/>") $$
		text "</HelpCollection>"
  writeFile (odir ++ pathSeparator:collectionHH2File) (render doc)
