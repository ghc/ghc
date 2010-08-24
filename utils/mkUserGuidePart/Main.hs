
module Main (main) where

import DynFlags

import Data.List
import System.Environment

main :: IO ()
main = do args <- getArgs
          case args of
              [] -> error "Need to give filename to generate as an argument"
              [f] ->
                  case f of
                      "docs/users_guide/users_guide.xml" ->
                          writeFile f userGuideMain
                      "docs/users_guide/what_glasgow_exts_does.gen.xml" ->
                          writeFile f whatGlasgowExtsDoes
                      _ ->
                          error ("Don't know what to do for " ++ show f)
              _ -> error "Bad args"

-- Hack: dblatex normalises the name of the main input file using
-- os.path.realpath, which means that if we're in a linked build tree,
-- it find the real source files rather than the symlinks in our link
-- tree. This is fine for the static sources, but it means it can't
-- find the generated sources.
-- We therefore also generate the main input file, so that it really
-- is in the link tree, and thus dblatex can find everything.
userGuideMain :: String
userGuideMain = unlines [
    "<?xml version=\"1.0\" encoding=\"iso-8859-1\"?>",
    "<!DOCTYPE book PUBLIC \"-//OASIS//DTD DocBook XML V4.2//EN\"",
    "   \"http://www.oasis-open.org/docbook/xml/4.2/docbookx.dtd\" [",
    "<!ENTITY % ug-ent SYSTEM \"ug-ent.xml\">",
    "%ug-ent;",
    "<!ENTITY ug-book SYSTEM \"ug-book.xml\">",
    "]>",
    "",
    "<book id=\"users-guide\">",
    "&ug-book;",
    "</book>"]

whatGlasgowExtsDoes :: String
whatGlasgowExtsDoes = case maybeInitLast glasgowExtsFlags of
                      Just (xs, x) ->
                          let xs' = map mkInitLine xs
                              x' = mkLastLine x
                          in unlines (xs' ++ [x'])
                      Nothing ->
                          error "glasgowExtsFlags is empty?"
    where mkInitLine = mkLine ','
          mkLastLine = mkLine '.'
          mkLine c f = case stripPrefix "Opt_" (show f) of
                       Just ext -> "<option>-X" ++ ext ++ "</option>" ++ [c]
                       Nothing -> error ("Can't parse extension: " ++ show f)

maybeInitLast :: [a] -> Maybe ([a], a)
maybeInitLast xs = case reverse xs of
                   (y : ys) -> Just (reverse ys, y)
                   _        -> Nothing

