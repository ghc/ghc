{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -dcore-lint #-}

module T21720 where

import Language.Haskell.TH

main :: IO ()
main = pure ()

$(do
    let noBang = Bang NoSourceUnpackedness NoSourceStrictness
    let mkData tn cn fn = DataD [] tn [] Nothing [RecC cn [(fn, noBang, ConT ''Integer)]] []
    r1 <- mkData <$> newName "R1" <*> newName "C1" <*> newName "f"
    r2 <- mkData <$> newName "R2" <*> newName "C2" <*> newName "f"
    pure [r1,r2]
 )
