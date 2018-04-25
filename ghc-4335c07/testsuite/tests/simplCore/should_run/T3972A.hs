
module T3972A (Expr(..), Token(..), spanning, getSpan) where

class Span a where
   getSpan :: a -> SrcSpan

spanning :: (Span a, Span b) => a -> b -> SrcSpan
spanning x y = combineSrcSpans (getSpan x) (getSpan y)

instance Span a => Span [a] where
   getSpan [] = error "[]"
   getSpan [x] = getSpan x
   getSpan list@(x:_) = combineSrcSpans (getSpan x) (getSpan (last list))

data SrcSpan

  = SpanMultiLine
    { span_start_row    :: !Int
    , span_start_column :: !Int
    , span_end_row      :: !Int
    , span_end_column   :: !Int
    }

combineSrcSpans :: SrcSpan -> SrcSpan -> SrcSpan
combineSrcSpans start end
 = case row1 `compare` row2 of
     LT -> SpanMultiLine row1 col1 row2 col2
     _  -> SpanMultiLine row2 col2 row1 col1
  where
  row1 = startRow start
  col1 = startCol start
  row2 = endRow end
  col2 = endCol end


startRow :: SrcSpan -> Int
startRow (SpanMultiLine { span_start_row = row }) = row

endRow :: SrcSpan -> Int
endRow (SpanMultiLine { span_end_row = row }) = row

startCol :: SrcSpan -> Int
startCol (SpanMultiLine { span_start_column = col }) = col

endCol :: SrcSpan -> Int
endCol (SpanMultiLine { span_end_column = col }) = col

data Token
   = T10 { token_SrcSpan :: SrcSpan }
   | T11 { token_SrcSpan :: SrcSpan }
   | T12 { token_SrcSpan :: SrcSpan }
   | T13 { token_SrcSpan :: SrcSpan }
   | T14 { token_SrcSpan :: SrcSpan }
   | T15 { token_SrcSpan :: SrcSpan }
   | T16 { token_SrcSpan :: SrcSpan }
   | T17 { token_SrcSpan :: SrcSpan }
   | T18 { token_SrcSpan :: SrcSpan }
   | T19 { token_SrcSpan :: SrcSpan }
   | T20 { token_SrcSpan :: SrcSpan }
   | T21 { token_SrcSpan :: SrcSpan }
   | T22 { token_SrcSpan :: SrcSpan }
   | T23 { token_SrcSpan :: SrcSpan }
   | T24 { token_SrcSpan :: SrcSpan }

instance Span Token where
  getSpan = token_SrcSpan

data Expr
   = E10 { expr_SrcSpan :: SrcSpan }
   | E11 { expr_SrcSpan :: SrcSpan }
   | E12 { expr_SrcSpan :: SrcSpan }
   | E13 { expr_SrcSpan :: SrcSpan }
   | E14 { expr_SrcSpan :: SrcSpan }
   | E15 { expr_SrcSpan :: SrcSpan }
   | E16 { expr_SrcSpan :: SrcSpan }
   | E17 { expr_SrcSpan :: SrcSpan }
   | E18 { expr_SrcSpan :: SrcSpan }
   | E19 { expr_SrcSpan :: SrcSpan }
   | E20 { expr_SrcSpan :: SrcSpan }
   | E21 { expr_SrcSpan :: SrcSpan }
   | E22 { expr_SrcSpan :: SrcSpan }
   | E23 { expr_SrcSpan :: SrcSpan }
   | E24 { expr_SrcSpan :: SrcSpan }

instance Span Expr where
   getSpan = expr_SrcSpan
