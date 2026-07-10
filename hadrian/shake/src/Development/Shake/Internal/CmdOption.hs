{-# LANGUAGE DeriveDataTypeable #-}
module Development.Shake.Internal.CmdOption(CmdOption(..)) where

import Data.Data
import qualified Data.ByteString.Lazy.Char8 as LBS

-- | Options passed to 'command' or 'cmd' to control how processes are executed.
data CmdOption
    = Cwd FilePath -- ^ Change the current directory in the spawned process. By default uses this processes current directory.
                   --   Successive 'Cwd' options are joined together, to change into nested directories.
    | Env [(String,String)] -- ^ Change the environment variables in the spawned process. By default uses this processes environment.
    | AddEnv String String -- ^ Add an environment variable in the child process.
    | RemEnv String -- ^ Remove an environment variable from the child process.
    | AddPath [String] [String] -- ^ Add some items to the prefix and suffix of the @$PATH@ variable.
    | Stdin String -- ^ Given as the @stdin@ of the spawned process.
    | StdinBS LBS.ByteString -- ^ Given as the @stdin@ of the spawned process.
    | FileStdin FilePath -- ^ Take the @stdin@ from a file.
    | Shell -- ^ Pass the command to the shell without escaping - any arguments will be joined with spaces. By default arguments are escaped properly.
    | BinaryPipes -- ^ Treat the @stdin@\/@stdout@\/@stderr@ messages as binary. By default 'String' results use text encoding and 'ByteString' results use binary encoding.
    | Traced String -- ^ Name to use with 'traced', or @\"\"@ for no tracing. By default traces using the name of the executable.
    | Timeout Double -- ^ Abort the computation after N seconds, will raise a failure exit code. Calls 'interruptProcessGroupOf' and 'terminateProcess', but may sometimes fail to abort the process and not timeout.
    | WithStdout Bool -- ^ Should I include the @stdout@ in the exception if the command fails? Defaults to 'False'.
    | WithStderr Bool -- ^ Should I include the @stderr@ in the exception if the command fails? Defaults to 'True'.
    | EchoStdout Bool -- ^ Should I echo the @stdout@? Defaults to 'True' unless a 'Stdout' result is required or you use 'FileStdout'.
    | EchoStderr Bool -- ^ Should I echo the @stderr@? Defaults to 'True' unless a 'Stderr' result is required or you use 'FileStderr'.
    | FileStdout FilePath -- ^ Should I put the @stdout@ to a file.
    | FileStderr FilePath -- ^ Should I put the @stderr@ to a file.
    | AutoDeps -- ^ Compute dependencies automatically. Only works if 'shakeLintInside' has been set to the files where autodeps might live.
    | UserCommand String -- ^ The command the user thinks about, before any munging. Defaults to the actual command.
    | FSAOptions String -- ^ Options to @fsatrace@, a list of strings with characters such as @\"r\"@ (reads) @\"w\"@ (writes). Defaults to @\"rwmdqt\"@ if the output of @fsatrace@ is required.
    | CloseFileHandles -- ^ Before starting the command in the child process, close all file handles except stdin, stdout, stderr in the child process. Uses @close_fds@ from package process and comes with the same caveats, i.e. runtime is linear with the maximum number of open file handles (@RLIMIT_NOFILE@, see @man 2 getrlimit@ on Linux).
    | NoProcessGroup -- ^ Don't run the process in its own group. Required when running @docker@. Will mean that process timeouts and asyncronous exceptions may not properly clean up child processes.
    | InheritStdin -- ^ Cause the stdin from the parent to be inherited. Might also require NoProcessGroup on Linux. Ignored if you explicitly pass a stdin.
      deriving (Eq,Ord,Show,Data,Typeable)
