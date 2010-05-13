-----------------------------------------------------------------------------
-- |
-- Module      :  Haddock.Backends.DevHelp
-- Copyright   :  (c) Simon Marlow 2003-2006,
--                    David Waern  2006
-- License     :  BSD-like
--
-- Maintainer  :  haddock@projects.haskell.org
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------
module Haddock.Backends.DevHelp (ppDevHelpFile) where

import Haddock.ModuleTree
import Haddock.Types hiding (Doc)
import Haddock.Utils

import Module
import Name          ( Name, nameModule, getOccString, nameOccName )

import Data.Maybe    ( fromMaybe )
import qualified Data.Map as Map
import Text.PrettyPrint

ppDevHelpFile :: FilePath -> String -> Maybe String -> [Interface] -> IO ()
ppDevHelpFile odir doctitle maybe_package ifaces = do
  let devHelpFile = package++".devhelp"
      tree = mkModuleTree True [ (ifaceMod iface, toDescription iface) | iface <- ifaces ]
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
    ppNode ss (Node s leaf _ _short ts) =
        case ts of
          [] -> text "<sub"<+>ppAttribs<>text "/>"
          _  -> 
            text "<sub"<+>ppAttribs<>text ">" $$
            nest 4 (ppModuleTree (s:ss) ts) $+$
            text "</sub>"
        where
          ppLink | leaf      = text (moduleHtmlFile (mkModule (stringToPackageId "") 
                                                              (mkModuleName mdl)))
                 | otherwise = empty

          ppAttribs = text "name="<>doubleQuotes (text s)<+>text "link="<>doubleQuotes ppLink

          mdl = foldr (++) "" (s' : map ('.':) ss')
          (s':ss') = reverse (s:ss)
		-- reconstruct the module name

    index :: [(Name, [Module])]
    index = Map.toAscList (foldr getModuleIndex Map.empty ifaces)

    getModuleIndex iface fm =
	Map.unionWith (++) (Map.fromListWith (flip (++)) [(name, [mdl]) | name <- ifaceExports iface, nameModule name == mdl]) fm
	where mdl = ifaceMod iface

    ppList :: [(Name, [Module])] -> Doc
    ppList [] = empty
    ppList ((name,refs):mdls)  =
      ppReference name refs $$
      ppList mdls

    ppReference :: Name -> [Module] -> Doc
    ppReference _ [] = empty
    ppReference name (mdl:refs) =  
      text "<function name=\""<>text (escapeStr (getOccString name))<>text"\" link=\""<>text (nameHtmlRef mdl (nameOccName name))<>text"\"/>" $$
      ppReference name refs
