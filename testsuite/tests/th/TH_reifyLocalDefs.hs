-- test reification of local definitions
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
import Language.Haskell.TH.Syntax
import System.IO

-- Sidestep the staging restriction
-- printTypeOf :: String -> Q ()
#define printTypeOf(n) (addModFinalizer $ do \
                       { VarI _ t _ <- reify (mkName (n)) \
                       ; runIO $ hPutStrLn stderr (n ++ " :: " ++ show t) \
                       })

main :: IO ()
main = print (f 1 "", g 'a' 2, h True 3)
  where
    f xf yf = ( xf :: Int
              , let ff $(do printTypeOf("yf")
                            [p| z |]
                        ) = z :: $(do printTypeOf("z")
                                      [t| () |]
                                  )
                 in $(do printTypeOf("xf")
                         [| yf :: String |]
                     )
            )
    g xg y = ( $(do printTypeOf("xg")
                    [| y :: Int |]
                )
             , xg :: Char
             )
    h xh y = ( $$(do printTypeOf("xh")
                     [|| y :: Int ||]
                 )
             , xh :: Bool
             )
