module Main where

import Network.URI
import Data.Maybe

main =  sequence_ (map do_test tests)

base = fromJust (parseURI "http://a/b/c/d;p?q")

do_test test = case parseURI test of
			Nothing -> error ("no parse: " ++ test)
			Just uri -> putStr (show (fromJust (uri `relativeTo` base)) ++ "\n")

tests =
  [   "g:h",
      "g",
      "./g",
      "g/",
      "/g",
      "//g",
      "?y",
      "g?y",
      "#s",
      "g#s",
      "g?y#s",
      ";x",
      "g;x",
      "g;x?y#s",
      ".",
      "./",
      "..",
      "../",
      "../g",
      "../..",
      "../../",
      "../../g",
      -- "../../../g" -- should fail
      -- "../../../../g" -- should fail
      "/./g",
      "/../g",
      "g.",
      ".g",
      "g..",
      "..g",
      "./../g",
      "./g/.",
      "g/./h",
      "g/../h",
      "g;x=1/./y",
      "g;x=1/../y",
      "g?y/./x",
      "g?y/../x",
      "g#s/./x",
      "g#s/../x"
  ]
