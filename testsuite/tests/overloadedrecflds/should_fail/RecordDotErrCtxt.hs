{-# LANGUAGE OverloadedRecordDot #-}

-- The renamer expands @e.fld@ to @getField \@"fld" e@ (mkGetField in
-- GHC.Rename.Expr).  For the sake of .hie files the application head of that
-- expansion carries the SrcSpan of the field label, but it must be a
-- *generated* span (@GeneratedSrcSpan (OrigSpan ...)@, built by wrapGenSpan')
-- rather than a plain RealSrcSpan, or else isGeneratedSrcSpan is False and
-- GHC.Tc.Gen.App starts reporting the internal 'getField' to the user.
--
-- The CtOrigin site (mk_origin) is already covered by T26480b, which contrasts
-- an explicit 'getField' ("arising from a use of `getField'") with record dot
-- syntax ("arising from selecting the field `x'"); see also T19843h and
-- RecordDotSyntaxFail8.  The result-type site is covered by
-- RecordDotSyntaxFail9.
--
-- What is *not* covered anywhere else is addArgCtxt (GHC.Tc.Gen.App), which
-- needs an ill-typed argument underneath the selection.  With a real head span
-- the second error below would gain a
--     "In the first argument of `getField', namely `('c' + 1)'"
-- context line.
module RecordDotErrCtxt where

data T = MkT { fld :: Int }

argCtxt :: Int
argCtxt = ('c' + 1).fld
