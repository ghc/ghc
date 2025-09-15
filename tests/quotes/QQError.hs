{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
module QQError where

main = [| [id|hello|] |]

main2 = [| [Prelude.id|hello|] |]

