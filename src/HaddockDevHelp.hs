module HaddockDevHelp(ppDevHelpFile) where

import HaddockModuleTree
import HaddockTypes
import HaddockUtil

import Module ( moduleString, Module )
import Name   ( Name, nameModule, getOccString )

import Data.Maybe ( fromMaybe )
import qualified Data.Map as Map
import Text.PrettyPrint

ppDevHelpFile :: FilePath -> String -> Maybe String -> [HaddockModule] -> IO ()
ppDevHelpFile odir doctitle maybe_package modules = do
  let devHelpFile = package++".devhelp"
      tree = mkModuleTree [ (hmod_mod mod, hmod_package mod, toDescription mod)
			    | mod <- modules ]
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

    index :: [(Name, [Module])]
    index = Map.toAscList (foldr getModuleIndex Map.empty modules)

    getModuleIndex hmod fm =
	Map.unionWith (++) (Map.fromListWith (flip (++)) [(name, [mod]) | name <- hmod_exports hmod, nameModule name == mod]) fm
	where mod = hmod_mod hmod

    ppList :: [(Name, [Module])] -> Doc
    ppList [] = empty
    ppList ((name,refs):mdls)  =
      ppReference name refs $$
      ppList mdls

    ppReference :: Name -> [Module] -> Doc
    ppReference name [] = empty
    ppReference name (mod:refs) = let modName = moduleString mod in 
      text "<function name=\""<>text (escapeStr (getOccString name))<>text"\" link=\""<>text (nameHtmlRef modName name)<>text"\"/>" $$
      ppReference name refs
