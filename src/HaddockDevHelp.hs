module HaddockDevHelp(ppDevHelpFile) where

import HsSyn hiding(Doc)

#if __GLASGOW_HASKELL__ < 503
import Pretty
import FiniteMap
#else
import Text.PrettyPrint
import Data.FiniteMap
import Data.Char
#endif

import Maybe	( fromMaybe )
import HaddockModuleTree
import HaddockUtil
import HaddockTypes


ppDevHelpFile :: FilePath -> String -> Maybe String -> [(Module,Interface)] -> IO ()
ppDevHelpFile odir doctitle maybe_package ifaces = do
  let devHelpFile = package++".devhelp"
      tree = mkModuleTree [(mod,iface_package iface) | (mod,iface) <- ifaces]
      doc =
        text "<?xml version=\"1.0\" encoding=\"utf-8\" standalone=\"no\"?>" $$
        (text "<book xmlns=\"http://www.devhelp.net/book\" title=\""<>text doctitle<>
            text "\" link=\""<>text contentsHtmlFile<>text"\" author=\"\" name=\""<>text package<>text "\">") $$
        text "<chapters>" $$
        nest 4 (ppModuleTree [] tree) $+$
        text "</chapters>" $$
        text "<functions>" $$
        nest 4 (ppList index) $+$
        text "</functions>" $$
        text "</book>"
  writeFile (odir ++ pathSeparator:devHelpFile) (render doc)
  where    
    package = fromMaybe "pkg" maybe_package

    ppModuleTree :: [String] -> [ModuleTree] -> Doc
    ppModuleTree ss [x]    = ppNode ss x
    ppModuleTree ss (x:xs) = ppNode ss x $$ ppModuleTree ss xs
    ppModuleTree _  []     = error "HaddockHH.ppHHContents.fn: no module trees given"

    ppNode :: [String] -> ModuleTree -> Doc
    ppNode ss (Node s leaf _pkg ts) =
        case ts of
          [] -> text "<sub"<+>ppAttribs<>text "/>"
          ts -> 
            text "<sub"<+>ppAttribs<>text ">" $$
            nest 4 (ppModuleTree (s:ss) ts) $+$
            text "</sub>"
        where
          ppLink | leaf      = text (moduleHtmlFile "" mdl)
                 | otherwise = empty

          ppAttribs = text "name="<>doubleQuotes (text s)<+>text "link="<>doubleQuotes ppLink

          mdl = foldr (++) "" (s' : map ('.':) ss')
          (s':ss') = reverse (s:ss)
		-- reconstruct the module name

    index :: [(HsName, [Module])]
    index = fmToList (foldr getIfaceIndex emptyFM ifaces)

    getIfaceIndex (mdl,iface) fm =
		addListToFM_C (++) fm [(name, [mdl]) | (name, Qual mdl' _) <- fmToList (iface_env iface), mdl == mdl']
	
    ppList [] = empty
    ppList ((name,refs):mdls)  =
      ppReference name refs $$
      ppList mdls

    ppReference name [] = empty
    ppReference name (Module mdl:refs) =
      text "<function name=\""<>text (escapeStr (show name))<>text"\" link=\""<>text (nameHtmlRef "" mdl name)<>text"\"/>" $$
      ppReference name refs
