-- *** what's left after removing data/type decls

-- I/O functions and definitions

module PreludeIO  where

import Cls
import Core
import IBool
import IChar
import IDouble
import IInt
import IInteger
import IList
import ITup0
import List		( (++), foldr )
import PS		( _PackedString, _unpackPS )
import Text
import TyArray
import TyComplex
import TyIO

-- File and channel names:

stdin	    =  "stdin"
stdout      =  "stdout"
stderr      =  "stderr"
stdecho     =  "stdecho"

done	      ::                                                Dialogue
readFile      :: String ->           FailCont -> StrCont     -> Dialogue
writeFile     :: String -> String -> FailCont -> SuccCont    -> Dialogue
appendFile    :: String -> String -> FailCont -> SuccCont    -> Dialogue
readBinFile   :: String ->           FailCont -> BinCont     -> Dialogue
writeBinFile  :: String -> Bin    -> FailCont -> SuccCont    -> Dialogue
appendBinFile :: String -> Bin    -> FailCont -> SuccCont    -> Dialogue
deleteFile    :: String ->           FailCont -> SuccCont    -> Dialogue
statusFile    :: String ->           FailCont -> StrCont     -> Dialogue
readChan      :: String ->           FailCont -> StrCont     -> Dialogue
appendChan    :: String -> String -> FailCont -> SuccCont    -> Dialogue
readBinChan   :: String ->           FailCont -> BinCont     -> Dialogue
appendBinChan :: String -> Bin    -> FailCont -> SuccCont    -> Dialogue
statusChan    :: String ->           FailCont -> StrCont     -> Dialogue
echo          :: Bool   ->           FailCont -> SuccCont    -> Dialogue
getArgs	      ::		     FailCont -> StrListCont -> Dialogue
getProgName   ::		     FailCont -> StrCont     -> Dialogue
getEnv	      :: String ->	     FailCont -> StrCont     -> Dialogue
setEnv	      :: String -> String -> FailCont -> SuccCont    -> Dialogue
sigAction     :: Int    -> SigAct -> FailCont -> SuccCont    -> Dialogue

done resps    =  []

readFile name fail succ resps =
     (ReadFile name) : strDispatch fail succ resps

writeFile name contents fail succ resps =
    (WriteFile name contents) : succDispatch fail succ resps

appendFile name contents fail succ resps =
    (AppendFile name contents) : succDispatch fail succ resps

readBinFile name fail succ resps =
    (ReadBinFile name) : binDispatch fail succ resps

writeBinFile name contents fail succ resps =
    (WriteBinFile name contents) : succDispatch fail succ resps

appendBinFile name contents fail succ resps =
    (AppendBinFile name contents) : succDispatch fail succ resps

deleteFile name fail succ resps =
    (DeleteFile name) : succDispatch fail succ resps

statusFile name fail succ resps =
    (StatusFile name) : strDispatch fail succ resps

readChan name fail succ resps =
    (ReadChan name) : strDispatch fail succ resps

appendChan name contents fail succ resps =
    (AppendChan name contents) : succDispatch fail succ resps

readBinChan name fail succ resps =
    (ReadBinChan name) : binDispatch fail succ resps

appendBinChan name contents fail succ resps =
    (AppendBinChan name contents) : succDispatch fail succ resps

statusChan name fail succ resps =
    (StatusChan name) : strDispatch fail succ resps

echo bool fail succ resps =
    (Echo bool) : succDispatch fail succ resps

getArgs fail succ resps =
	GetArgs : strListDispatch fail succ resps

getProgName fail succ resps =
	GetProgName : strDispatch fail succ resps

getEnv name fail succ resps =
	(GetEnv name) : strDispatch fail succ resps

setEnv name val fail succ resps =
	(SetEnv name val) : succDispatch fail succ resps

sigAction signal action fail succ resps =
	(SigAction signal action) : succDispatch fail succ resps

strDispatch fail succ (resp:resps) = 
            case resp of Str val     -> succ val resps
                         Failure msg -> fail msg resps

strListDispatch fail succ (resp:resps) = 
            case resp of StrList val -> succ val resps
                         Failure msg -> fail msg resps

binDispatch fail succ (resp:resps) = 
            case resp of Bn val      -> succ val resps
                         Failure msg -> fail msg resps

succDispatch fail succ (resp:resps) = 
            case resp of Success     -> succ resps
                         Failure msg -> fail msg resps


abort		:: FailCont
abort err	=  done

exit		:: FailCont
exit err	= appendChan stderr (msg ++ "\n") abort done
		  where msg = case err of ReadError s   -> s
		  			  WriteError s  -> s
		  			  SearchError s -> s
		      			  FormatError s -> s
		      			  OtherError s  -> s
					  EOD		-> "EOD"

{-# GENERATE_SPECS print a{+,(),Bool,Char,Int,Integer,Double,_PackedString,[Char],[Int],[[Char]],[[Int]]} #-} 
print		:: (Text a) => a -> Dialogue
print x		=  appendChan stdout (show x) exit done

{-# GENERATE_SPECS prints a{+,(),Bool,Char,Int,Integer,Double,_PackedString,[Char],[Int],[[Char]],[[Int]]} #-} 
prints          :: (Text a) => a -> String -> Dialogue
prints x s	=  appendChan stdout (shows x s) exit done

interact	:: (String -> String) -> Dialogue
interact f	=  readChan stdin exit
			    (\x -> appendChan stdout (f x) exit done)
