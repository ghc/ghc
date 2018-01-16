-- | Datatypes for law QuickChecks

-- User visible combinators. The tests are performed on pretty printing terms
-- which are constructable using the public combinators.  We need to have a
-- datatype for those combinators, otherwise it becomes almost impossible to
-- reconstruct failing tests.
--
module TestStructures (
        CDoc(..), CList(..), CDocList(..), Text(..),

        buildDoc, liftDoc2, liftDoc3, buildDocList,
        text', annotToTd, tdToStr, genericCProp
    ) where

import PrettyTestVersion

data CDoc = CEmpty           -- empty
          | CText String     -- text s
          | CList CList [CDoc] -- cat,sep,fcat,fsep ds
          | CBeside Bool CDoc CDoc -- a <> b and a <+> b
          | CAbove Bool CDoc CDoc  -- a $$ b and a $+$ b
          | CNest Int CDoc   -- nest k d
    deriving (Eq, Ord)

data CList = CCat | CSep | CFCat | CFSep deriving (Eq,Ord)

newtype CDocList = CDocList { unDocList :: [CDoc] } 

-- wrapper for String argument of `text'
newtype Text = Text { unText :: String } deriving (Eq, Ord, Show)

instance Show CDoc where
    showsPrec k CEmpty = showString "empty"
    showsPrec k (CText s) = showParen (k >= 10) (showString " text " . shows s)
    showsPrec k (CList sp ds) = showParen (k >= 10) $ (shows sp . showList ds)
    showsPrec k (CBeside sep d1 d2) = showParen (k >= 6) $ 
        (showsPrec 6 d1) . showString (if sep then " <+> " else " <> ") . (showsPrec 6 d2) 
    showsPrec k (CAbove noOvlap d1 d2) = showParen (k >= 5) $ 
        (showsPrec 5 d1) . showString (if noOvlap then " $+$ " else " $$ ") . (showsPrec 5 d2) 
    showsPrec k (CNest n d) = showParen (k >= 10) $ showString " nest " . showsPrec 10 n . showString " ". showsPrec 10 d

instance Show CList where 
    show cs = case cs of CCat -> "cat" ;  CSep -> "sep" ; CFCat -> "fcat"  ; CFSep -> "fsep" 

instance Show CDocList where show = show . unDocList
 
buildDoc :: CDoc -> Doc ()
buildDoc CEmpty = empty
buildDoc (CText s) = text s
buildDoc (CList sp ds) = (listComb sp) $ map buildDoc ds
buildDoc (CBeside sep d1 d2) = (if sep then (<+>) else (<>)) (buildDoc d1) (buildDoc d2) 
buildDoc (CAbove noOvlap d1 d2) = (if noOvlap then ($+$) else ($$)) (buildDoc d1) (buildDoc d2) 
buildDoc (CNest k d) = nest k $ buildDoc d

listComb :: CList -> ([Doc ()] -> Doc ())
listComb cs = case cs of CCat -> cat ;  CSep -> sep ; CFCat -> fcat  ; CFSep -> fsep

liftDoc2 :: (Doc () -> Doc () -> a) -> (CDoc -> CDoc -> a)
liftDoc2 f cd1 cd2 = f (buildDoc cd1) (buildDoc cd2)

liftDoc3 :: (Doc () -> Doc () -> Doc () -> a) -> (CDoc -> CDoc -> CDoc -> a)
liftDoc3 f cd1 cd2 cd3 = f (buildDoc cd1) (buildDoc cd2) (buildDoc cd3)
    
buildDocList :: CDocList -> [Doc ()]
buildDocList = map buildDoc . unDocList

text' :: Text -> Doc ()
text' (Text str) = text str

annotToTd :: AnnotDetails a -> TextDetails
annotToTd (NoAnnot s _) = s
annotToTd _             = Str ""

-- convert text details to string
tdToStr :: TextDetails -> String
tdToStr (Chr c) = [c]
tdToStr (Str s) = s
tdToStr (PStr s) = s

-- synthesize with stop for cdoc
-- constructor order
genericCProp :: (a -> a -> a) -> (CDoc -> (a, Bool)) -> CDoc -> a
genericCProp c q cdoc = 
    case q cdoc of
        (v,False) -> v
        (v,True)  -> foldl c v subs
    where
        rec = genericCProp c q
        subs = case cdoc of
            CEmpty  -> []
            CText _ -> []
            CList _ ds -> map rec ds
            CBeside _ d1 d2 -> [rec d1, rec d2]
            CAbove b d1 d2 -> [rec d1, rec d2]
            CNest k d -> [rec d]

