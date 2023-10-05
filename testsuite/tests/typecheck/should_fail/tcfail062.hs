-- !!! bug report from Satnam
--
module ShouldFail
where

type Module = (String,[Declaration])

data Declaration
  = Architecture String StructuralExpression |
    Behaviour String Parameter Parameter BehaviouralExpression
    deriving (Eq, Show)

data Parameter = ParameterVariable String | ParameterList [Parameter]
                 deriving (Eq, Show)

nameOfModule :: Module -> String
nameOfModule (name, _) = name

data StructuralExpression 
  = Variable String |
    Serial StructuralExpression StructuralExpression | 
    Par [StructuralExpression] 
    deriving (Eq, Show)

data BehaviouralExpression
  = BehaviouralVariable String 
    | AndExpr BehaviouralExpression BehaviouralExpression
    | OrExpr BehaviouralExpression BehaviouralExpression
    | NotExpr BehaviouralExpression
    deriving (Eq, Show)


type BehaviouralRelation
  = (behaviouralExpression, behaviouralExpression)
---- ^ typo ----------------^ typo (but so what?)

type BehaviouralRelationList = [BehaviouralRelation]
