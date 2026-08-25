{-# OPTIONS -ddump-parsed-ast #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Test20297 where

-- Each declaration below places a comment between a token (or other
-- output) and a following markAnnListA call in
-- utils/check-exact/ExactPrint.hs, where the AnnList carries
-- EpVirtualBraces.
--
-- markAnnListA does pushListLayout/setLayoutBoth before running its
-- action, so the comment is flushed by printCommentsBefore
-- (ExactPrint.hs:424) inside the pushed layout.  It is offset against
-- the list's dLHS rather than the enclosing one, and it is consumed
-- before priorEndAfterComments is read at ExactPrint.hs:461 for the
-- list anchor.  Under makeDeltaAst the comment is therefore pushed
-- onto its own line at the list indentation.
--
-- Cases marked BROKEN currently move under
--   check-exact <libdir> Test20297.hs makeDelta
-- Cases marked OK round-trip unchanged.

-- OK.  HsValBinds (ExactPrint.hs:2444) with 'where' and no decls at
-- all: nothing prints inside the push, so the comment is flushed after
-- popListLayout, in the enclosing layout.
bar = x
  -- comment0
  where -- comment1

-- BROKEN.  HsValBinds (ExactPrint.hs:2444), 'where' at :2442.
-- comment2 is captured as a preceding comment of doStuff.
foo = x
  where -- comment2
        doStuff = do stuff

-- BROKEN.  HsIPBinds (ExactPrint.hs:2450), 'where' at :2449.
ipb = ?y
  where -- comment3
        ?y = 1

-- BROKEN.  ClsInstDecl (ExactPrint.hs:2183), 'where' printed at the
-- end of top_matter (:2196).
instance C Int where -- comment4
  meth = x

-- BROKEN.  ClassDecl, laid out (ExactPrint.hs:3517), 'where' at the
-- end of top_matter (:3537).
class C a where -- comment5
  meth :: a

-- OK.  ClassDecl with null decls (ExactPrint.hs:3506): the action is
-- 'return ()', so as with bar above nothing prints inside the push.
class D a where -- comment6

-- BROKEN.  FamilyDecl/ClosedTypeFamily (ExactPrint.hs:3591), 'where'
-- at :3588.
type family F a where -- comment7
  F Int = Bool

-- BROKEN.  exact_condecls in GADT syntax (ExactPrint.hs:4132).  The
-- 'where' is printed by the caller, exactDataDefn:3675.
data G a where -- comment8
  MkG :: G Int

-- BROKEN.  HsMultiIf (ExactPrint.hs:2868), 'if' at :2867.
mwi = if -- comment9
         | x -> 1
         | otherwise -> 2

-- HsUntypedBracket/DecBrL (ExactPrint.hs:2969), the only site with a
-- token on both sides of the list.
-- BROKEN for comment10, after the opening '[d|' at :2968.
-- OK for comment11, before the closing '|]' at :2970: by then
-- popListLayout has already run.
declBracket = [d| -- comment10
                  dee = 1
                  -- comment11
                |]

-- BROKEN.  markMaybeDodgyStmts (ExactPrint.hs:3083), 'do' printed by
-- exactDo:3053.  The LocatedA around the stmt list is *inside* the
-- push, so comment12 lands on the list wrapper rather than on stmt1,
-- but it is still offset against the list layout.
dodo = do -- comment12
          stmt1
          stmt2

-- OK.  Same site, but comment13 is already on its own line at the list
-- indentation, so the damage is not visible in the output.  The
-- recorded delta is still taken against the list layout.
dodo2 = do
    -- comment13
    stmt1

-- BROKEN.  RecStmt (ExactPrint.hs:3418), 'rec' at :3417.
recdo = do
  rec -- comment14
      a <- f b
      b <- g a
  return a

-- BROKEN.  HsCmdDo via markAnnListD (ExactPrint.hs:3367), 'do' at
-- :3366.
cmd = proc x -> do -- comment15
                   returnA -< x

-- OK.  MatchGroup (ExactPrint.hs:3156).  The one site that is already
-- correct, and it shows the invariant the others need: an enterAnn
-- anchored at the list start must be the *last thing printed* before
-- markAnnListA.
--
-- getAnnotationEntry (MG _ (L l _)) = fromAnn l enters on the alts
-- anchor, and 'exact' then prints nothing but the markAnnListA call, so
-- comment16 is flushed outside the push.  (The L l itself is otherwise
-- inert: markAnnotated is applied to the bare 'matches' list.)
--
-- The BROKEN cases fail against one half or the other.  The 'where'
-- sites have no entry at the list anchor at all -- the enclosing
-- LocatedA is anchored at the 'where'.  The do/rec sites do have one,
-- but it is sequenced inside the action, after the push.
mg y = case y of -- comment16
         1 -> a
         _ -> b
