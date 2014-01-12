{-# LANGUAGE TemplateHaskell #-}

module T5362() where

import System.IO
import Language.Haskell.TH

$(
     do fName <- newName "f"
        gName <- newName "g"
        let gExp = varE gName

        sdf <- sigD fName [t| () |]
        sdg <- sigD gName [t| () |]
        fdf <- funD fName [clause [] (normalB [| undefined $gExp |]) []]
        fdg <- funD gName [clause [] (normalB [| undefined       |]) []]
        let ds = [sdf, fdf, sdg, fdg]
        runIO $ do { putStrLn (pprint ds); hFlush stdout }
        return ds
     )



