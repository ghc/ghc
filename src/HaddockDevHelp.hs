module HaddockDevHelp(ppDevHelpFile) where

import HaddockModuleTree
import HaddockTypes
import HaddockUtil
import HsSyn hiding(Doc)
import qualified Map

import Data.Maybe ( fromMaybe )
import Text.PrettyPrint


ppDevHelpFile :: FilePath -> String -> Maybe String -> [Interface] -> IO ()
ppDevHelpFile odir doctitle maybe_package ifaces = do
  let devHelpFile = package++".devhelp"
      tree = mkModuleTree [ (iface_module iface, 
			     iface_package iface, 
			     toDescription iface)
			  | iface <- ifaces ]
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
  writeFile (pathJoin [odir, devHelpFile]) (render doc)
  where    
    package = fromMaybe "pkg" maybe_package

    ppModuleTree :: [String] -> [ModuleTree] -> Doc
    ppModuleTree ss [x]    = ppNode ss x
    ppModuleTree ss (x:xs) = ppNode ss x $$ ppModuleTree ss xs
    ppModuleTree _  []     = error "HaddockHH.ppHHContents.fn: no module trees given"

    ppNode :: [String] -> ModuleTree -> Doc
    ppNode ss (Node s leaf _pkg _short ts) =
        case ts of
          [] -> text "<sub"<+>ppAttribs<>text "/>"
          ts -> 
            text "<sub"<+>ppAttribs<>text ">" $$
            nest 4 (ppModuleTree (s:ss) ts) $+$
            text "</sub>"
        where
          ppLink | leaf      = text (moduleHtmlFile mdl)
                 | otherwise = empty

          ppAttribs = text "name="<>doubleQuotes (text s)<+>text "link="<>doubleQuotes ppLink

          mdl = foldr (++) "" (s' : map ('.':) ss')
          (s':ss') = reverse (s:ss)
		-- reconstruct the module name

    index :: [(HsName, [Module])]
    index = Map.toAscList (foldr getIfaceIndex Map.empty ifaces)

    getIfaceIndex iface fm =
		Map.unionWith (++) (Map.fromListWith (flip (++)) [(name, [mdl]) | (name, Qual mdl' _) <- Map.toAscList (iface_env iface), mdl == mdl']) fm
		where mdl = iface_module iface

    ppList [] = empty
    ppList ((name,refs):mdls)  =
      ppReference name refs $$
      ppList mdls

    ppReference name [] = empty
    ppReference name (Module mdl:refs) =
      text "<function name=\""<>text (escapeStr (show name))<>text"\" link=\""<>text (nameHtmlRef mdl name)<>text"\"/>" $$
      ppReference name refs
