{-# language TemplateHaskellQuotes #-}
module T20969A where
import Data.Sequence.Internal
import qualified Language.Haskell.TH.Syntax as TH

class Functor t => SequenceCode t where
  traverseCode :: TH.Quote m => (a -> TH.Code m b) -> t a -> TH.Code m (t b)
  traverseCode f = sequenceCode . fmap f
  sequenceCode :: TH.Quote m => t (TH.Code m a) -> TH.Code m (t a)
  sequenceCode = traverseCode id

instance SequenceCode Seq where
  sequenceCode (Seq t) = [|| Seq $$(traverseCode sequenceCode t) ||]

instance SequenceCode Elem where
  sequenceCode (Elem t) = [|| Elem $$t ||]

instance SequenceCode FingerTree where
  sequenceCode (Deep s pr m sf) =
    [|| Deep s $$(sequenceCode pr) $$(traverseCode sequenceCode m) $$(sequenceCode sf) ||]
  sequenceCode (Single a) = [|| Single $$a ||]
  sequenceCode EmptyT = [|| EmptyT ||]

instance SequenceCode Digit where
  sequenceCode (One a) = [|| One $$a ||]
  sequenceCode (Two a b) = [|| Two $$a $$b ||]
  sequenceCode (Three a b c) = [|| Three $$a $$b $$c ||]
  sequenceCode (Four a b c d) = [|| Four $$a $$b $$c $$d ||]

instance SequenceCode Node where
  sequenceCode (Node2 s x y) = [|| Node2 s $$x $$y ||]
  sequenceCode (Node3 s x y z) = [|| Node3 s $$x $$y $$z ||]
