module PreludeIO (
	Request(..), Response(..), IOError(..),
	Dialogue(..), SigAct(..),
	SuccCont(..), StrCont(..), StrListCont(..),
	BinCont(..), FailCont(..)
    ) where

import UTypes

-- Requests and responses:

data Request =	-- file system requests:
			  ReadFile      String         
			| WriteFile     String String
			| AppendFile    String String
			| ReadBinFile   String 
			| WriteBinFile  String Bin
			| AppendBinFile String Bin
			| DeleteFile    String
			| StatusFile    String
		-- channel system requests:
			| ReadChan	String 
			| AppendChan    String String
			| ReadBinChan   String 
			| AppendBinChan String Bin
			| StatusChan    String
		-- environment requests:
			| Echo          Bool
			| GetArgs
			| GetProgName
			| GetEnv        String
			| SetEnv        String String
			| SigAction     Int    SigAct
		deriving () -- NB: Text

data SigAct = 	    	  SAIgnore
    	    	    	| SADefault
    	    	    	| SACatch Dialogue

data Response =		  Success
			| Str String 
			| StrList [String]
			| Bn  Bin
			| Failure IOError
		deriving () -- NB: Text

data IOError =		  WriteError   String
			| ReadError    String
			| SearchError  String
			| FormatError  String
			| OtherError   String
			| EOD -- 1.3
		deriving () -- NB: Text


-- Continuation-based I/O:

type Dialogue    =  [Response] -> [Request]
type SuccCont    =                Dialogue
type StrCont     =  String     -> Dialogue
type StrListCont =  [String]   -> Dialogue
type BinCont     =  Bin        -> Dialogue
type FailCont    =  IOError    -> Dialogue
