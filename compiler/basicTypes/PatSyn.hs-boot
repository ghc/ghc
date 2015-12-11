module PatSyn where
import Name( NamedThing )
import Data.Typeable ( Typeable )
import Data.Data ( Data )
import Outputable ( Outputable, OutputableBndr )
import Unique ( Uniquable )
import BasicTypes (Arity)
import {-# SOURCE #-} TyCoRep (Type)
import Var (TyVar)
import Name (Name)

data PatSyn

patSynArity :: PatSyn -> Arity
patSynInstArgTys :: PatSyn -> [Type] -> [Type]
patSynExTyVars :: PatSyn -> [TyVar]
patSynName :: PatSyn -> Name



instance Eq PatSyn
instance Ord PatSyn
instance NamedThing PatSyn
instance Outputable PatSyn
instance OutputableBndr PatSyn
instance Uniquable PatSyn
instance Typeable PatSyn
instance Data PatSyn
