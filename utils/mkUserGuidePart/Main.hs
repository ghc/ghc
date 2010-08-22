
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
                      "docs/users_guide/what_glasgow_exts_does.gen.xml" ->
                          writeFile f whatGlasgowExtsDoes
                      _ ->
                          error ("Don't know what to do for " ++ show f)
              _ -> error "Bad args"

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

