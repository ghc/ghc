module Company where

import Text.XML.HaXml.Xml2Haskell
import Text.XML.HaXml.OneOfN


{-Type decls-}

newtype Company = Company [Dept] 		deriving (Eq,Show)
data Dept = Dept Name Manager [Unit]
	  deriving (Eq,Show)
data Unit = UnitEmployee Employee
	  | UnitDept Dept
	  deriving (Eq,Show)
data Employee = Employee Person Salary
	      deriving (Eq,Show)
data Person = Person Name Address
	    deriving (Eq,Show)
newtype Salary = Salary String 		deriving (Eq,Show)
newtype Manager = Manager Employee 		deriving (Eq,Show)
newtype Name = Name String 		deriving (Eq,Show)
newtype Address = Address String 		deriving (Eq,Show)


{-Instance decls-}

instance XmlContent Company where
    fromElem (CElem (Elem "company" [] c0):rest) =
	(\(a,ca)->
	   (Just (Company a), rest))
	(many fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Company a) =
	[CElem (Elem "company" [] (concatMap toElem a))]
instance XmlContent Dept where
    fromElem (CElem (Elem "dept" [] c0):rest) =
	(\(a,ca)->
	   (\(b,cb)->
	      (\(c,cc)->
		 (Just (Dept a b c), rest))
	      (many fromElem cb))
	   (definite fromElem "<manager>" "dept" ca))
	(definite fromElem "<name>" "dept" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Dept a b c) =
	[CElem (Elem "dept" [] (toElem a ++ toElem b ++
				concatMap toElem c))]
instance XmlContent Unit where
    fromElem (CElem (Elem "unit" [] c0):rest) =
	case (fromElem c0) of
	(Just a,_) -> (Just (UnitEmployee a), rest)
	(_,_) ->
		case (fromElem c0) of
		(Just a,_) -> (Just (UnitDept a), rest)
		(_,_) ->
		    (Nothing, c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (UnitEmployee a) = [CElem (Elem "unit" [] (toElem a) )]
    toElem (UnitDept a) = [CElem (Elem "unit" [] (toElem a) )]
instance XmlContent Employee where
    fromElem (CElem (Elem "employee" [] c0):rest) =
	(\(a,ca)->
	   (\(b,cb)->
	      (Just (Employee a b), rest))
	   (definite fromElem "<salary>" "employee" ca))
	(definite fromElem "<person>" "employee" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Employee a b) =
	[CElem (Elem "employee" [] (toElem a ++ toElem b))]
instance XmlContent Person where
    fromElem (CElem (Elem "person" [] c0):rest) =
	(\(a,ca)->
	   (\(b,cb)->
	      (Just (Person a b), rest))
	   (definite fromElem "<address>" "person" ca))
	(definite fromElem "<name>" "person" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Person a b) =
	[CElem (Elem "person" [] (toElem a ++ toElem b))]
instance XmlContent Salary where
    fromElem (CElem (Elem "salary" [] c0):rest) =
	(\(a,ca)->
	   (Just (Salary a), rest))
	(definite fromText "text" "salary" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Salary a) =
	[CElem (Elem "salary" [] (toText a))]
instance XmlContent Manager where
    fromElem (CElem (Elem "manager" [] c0):rest) =
	(\(a,ca)->
	   (Just (Manager a), rest))
	(definite fromElem "<employee>" "manager" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Manager a) =
	[CElem (Elem "manager" [] (toElem a))]
instance XmlContent Name where
    fromElem (CElem (Elem "name" [] c0):rest) =
	(\(a,ca)->
	   (Just (Name a), rest))
	(definite fromText "text" "name" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Name a) =
	[CElem (Elem "name" [] (toText a))]
instance XmlContent Address where
    fromElem (CElem (Elem "address" [] c0):rest) =
	(\(a,ca)->
	   (Just (Address a), rest))
	(definite fromText "text" "address" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Address a) =
	[CElem (Elem "address" [] (toText a))]


{-Done-}
