--!!! bug report from Satnam
--
module RubyAST
where

type Module = (String,[Declaration])

data Declaration
  = Architecture String StructuralExpression |
    Behaviour String Parameter Parameter BehaviouralExpression
    deriving (Eq, Text)

data Parameter = ParameterVariable String | ParameterList [Parameter]
                 deriving (Eq, Text)

nameOfModule :: Module -> String
nameOfModule (name, _) = name

data StructuralExpression 
  = Variable String |
    Serial StructuralExpression StructuralExpression | 
    Par [StructuralExpression] 
    deriving (Eq, Text)

data BehaviouralExpression
  = BehaviouralVariable String 
    | AndExpr BehaviouralExpression BehaviouralExpression
    | OrExpr BehaviouralExpression BehaviouralExpression
    | NotExpr BehaviouralExpression
    deriving (Eq, Text)


type BehaviouralRelation
  = (behaviouralExpression, behaviouralExpression)
-----^ typo ----------------^ typo (but so what?)

type BehaviouralRelationList = [BehaviouralRelation]
