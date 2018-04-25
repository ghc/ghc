{-# LANGUAGE GADTs #-}

module DPH.War.Result
	( Result(..)
	, isResultUnexpectedFailure
	, isResultUnexpectedSuccess
	, takeResultTime
	, takeResultDiff
	, takeQuirks )
where
import BuildBox
import Data.Maybe

data Result
	= ResultUnexpectedFailure
	| ResultUnexpectedSuccess
	| ResultAspect 	(WithUnits (Aspect Single))
	| ResultQuirk  	Quirk
	| ResultDiff   	FilePath FilePath FilePath
	deriving Show

isResultUnexpectedFailure :: Result -> Bool
isResultUnexpectedFailure rr
 = case rr of
	ResultUnexpectedFailure{}	-> True
	_				-> False

isResultUnexpectedSuccess :: Result -> Bool
isResultUnexpectedSuccess rr
 = case rr of
	ResultUnexpectedSuccess{}	-> True
	_				-> False

takeResultTime :: [Result] -> Maybe Seconds
takeResultTime as
 	= listToMaybe [t | ResultAspect (WithSeconds (Time TotalWall (Single t))) <- as]


takeResultDiff :: [Result] -> Maybe (FilePath, FilePath, FilePath)
takeResultDiff as
 = listToMaybe 	[ (fileRef, fileOut, fileDiff) 
		| a@(ResultDiff fileRef fileOut fileDiff) <- as]


takeQuirks :: [Result] -> [Quirk]
takeQuirks rs
	= [q | ResultQuirk q <- rs]
