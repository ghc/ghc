{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.TH.Syntax

$(do
    addForeignSource LangC ""
    addForeignSource LangCxx ""
    pure []
    )

main :: IO ()
main = pure ()
