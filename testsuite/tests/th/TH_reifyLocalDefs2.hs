-- Tests that a complete type is yielded by reify for local definitions,
-- even when using functional dependencies which are resolved at the very end of
-- type checking.
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
module TH_reifyLocalDefs2 where
import Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax as TH
import System.IO

class C a b | a -> b where
  yo :: a -> IO b

instance C Bool Int where
  yo _ = return 0

t3 :: IO ()
t3 = do
  x <- yo True
  $(do addModFinalizer $ do
         VarI _ t _ <- TH.reify 'x
         runIO $ hPutStrLn stderr $ show t
       [| return () |]
   )
