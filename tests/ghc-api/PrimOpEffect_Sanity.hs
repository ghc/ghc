import GHC.Builtin.PrimOps
import GHC.Utils.Outputable

main :: IO ()
main = let
  errs = do
    op <- allThePrimOps
    case  primOpEffect op  of
      NoEffect -> []
      CanFail  -> []
      ThrowsException -> []
      ReadWriteEffect
        -> [ppr op <+> text "has ReadWriteEffect but is work-free"
           | primOpIsWorkFree op]
        ++ [ppr op <+> text "has ReadWriteEffect but is cheap"
           | primOpIsCheap op]
  in  putStrLn $ showSDocUnsafe (vcat errs)
