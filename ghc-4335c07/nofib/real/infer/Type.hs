module Type
       (TVarId, TConId,
        MonoType (TVar, TCon), arrow,
        PolyType (All),
        freeTVarMono, freeTVarPoly)
       where

import Parse
import Shows
import MyList
import Data.List(nub)--1.3

type  TVarId          =  String
type  TConId          =  String
data  MonoType        =  TVar TVarId
                      |  TCon TConId [MonoType]
--ToDo:               deriving (Eq)

data  PolyType        =  All [TVarId] MonoType
u `arrow` v           =  TCon "->" [u,v]
freeTVarMono                  :: MonoType -> [TVarId]
freeTVarMono (TVar x)         =  [x]
freeTVarMono (TCon k ts)      =  concat (map freeTVarMono ts)
freeTVarPoly                  :: PolyType -> [TVarId]
freeTVarPoly (All xs t)       =  nub (freeTVarMono t) `minus` xs

-- WDP: too bad deriving doesn't work
instance Eq MonoType where
    (TVar tv1)       == (TVar tv2)	 = tv1 == tv2
    (TCon tc1 args1) == (TCon tc2 args2) = tc1 == tc2 && (args1 == args2)
    other1	     == other2		 = False
-- end of too bad

instance  Read MonoType  where
      readsPrec d     =  readsMono d
instance  Show MonoType  where
      showsPrec d     =  showsMono d

readsMono             :: Int -> Parses MonoType
readsMono d           =       ((d<=1) `guardP` readsArrow)
                      `elseP` ((d<=9) `guardP` readsTCon)
                      `elseP` (readsTVar)
                      `elseP` (parenP (readsMono 0))

readsArrow            :: Parses MonoType
readsArrow            =  readsMono 2          `thenP` (\u ->
                         lexP "->"            `thenP` (\_ ->
                         readsMono 1          `thenP` (\v ->
                                              returnP (u `arrow` v))))
readsTCon             :: Parses MonoType
readsTCon             =  readsTConId          `thenP` (\k  ->
                         starP (readsMono 10) `thenP` (\ts ->
                                              returnP (TCon k ts)))
readsTVar             :: Parses MonoType
readsTVar             =  readsTVarId          `thenP` (\x ->
                                              returnP (TVar x))
readsTVarId           :: Parses String
readsTVarId           =  lexicalP (lowerP `consP` starP alphaP)
readsTConId           :: Parses String
readsTConId           =  lexicalP (upperP `consP` starP alphaP)
showsMono             :: Int -> Shows MonoType
showsMono d (TVar xx)
      =  showsString xx
showsMono d (TCon "->" [uu,vv])
      =  showsParenIf (d>1)
         (showsMono 2 uu . showsString " -> " . showsMono 1 vv)
showsMono d (TCon kk tts)
      =  showsParenIf (d>9)
         (showsString kk .
          showsStar (\tt -> showsString " " . showsMono 10 tt) tts)
instance  Read PolyType  where
      readsPrec d             =  reads `eachP` polyFromMono
instance  Show PolyType  where
      showsPrec d (All xs t)  =  showsString "All " . showsString (unwords xs) .
                                 showsString ". " . showsMono 0 t
polyFromMono          :: MonoType -> PolyType
polyFromMono t        =  All (nub (freeTVarMono t)) t
