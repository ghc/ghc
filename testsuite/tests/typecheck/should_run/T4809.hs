{- # LANGUAGE MonoLocalBinds # -}
module Main where

import T4809_IdentityT (IdentityT(..), XML, runIdentityT)
import T4809_XMLGenerator (XMLGenT(..), XMLGen(genElement), Child, EmbedAsChild(..), unXMLGenT)
import System.IO (BufferMode(..), hSetBuffering, stdout)

page :: XMLGenT (IdentityT IO) XML
page = genElement (Nothing, "ul") [] [ asChild (asChild "foo")]
--    where
--       item :: XMLGenT (IdentityT IO) [Child (IdentityT IO)]
--       item = (asChild $ asChild (return "bar" :: XMLGenT (IdentityT IO) String))

main :: IO ()
main =
    do hSetBuffering stdout LineBuffering 
       r <- runIdentityT (unXMLGenT page)
       print r
