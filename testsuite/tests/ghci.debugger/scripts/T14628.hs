module T14628 where

import System.IO
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Text.Printf

putArrayBytes :: Handle       -- ^ output file handle
              -> [String]     -- ^ byte-strings
              -> IO Int       -- ^ total number of bytes written
putArrayBytes outfile xs = do
  let writeCount x = modify' (+ length x) >> liftIO (putLine x) :: MonadIO m => StateT Int m ()
  execStateT (mapM_ writeCount xs) 0
  where putLine = hPutStrLn outfile . ("  "++) . concatMap (printf "0x%02X,")
