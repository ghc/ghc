{-# LANGUAGE TemplateHaskell #-}

module T2700 where
import Language.Haskell.TH
import System.IO

$( do { d <- sigD (mkName "foo") [t| (Int -> Bool) -> Bool |]
      ; runIO (hPutStrLn stderr (pprint d))
      ; return [] }
  )
