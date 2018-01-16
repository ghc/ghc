
module Data.Array.Parallel.Unlifted.Distributed.What
        ( Comp  (..)
        , What  (..))
where
        
-- | What distributed computation we are doing.
data Comp
        = CGen          Bool            -- cheap
                        What

        | CMap          What
        | CFold         What
        | CScan         What
        | CDist         What
        deriving (Eq,Ord,Read,Show)

-- | What sort of thing is being computed.
data What
        = What          String
        | WScalar 
        | WZip
        | WSlice
        | WLength
        | WLengthIdx
        | WBpermute

        -- Copy due to a join instruction.
        | WJoinCopy     Int             -- number elements

        | WFMapMap      What What
        | WFMapGen      What What
        | WFZipMap      What What
        deriving (Eq,Ord,Read,Show)
