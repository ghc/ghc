module Language.Haskell.Syntax.InlinePragma where 

import GHC.Prelude
import Data.Data 
import Language.Haskell.Syntax.Basic(Arity)
import Language.Haskell.Syntax.Extension

data InlinePragma p           -- Note [InlinePragma] in GHC.Hs.InlinePragma
  = InlinePragma
      { inl_ext    :: XInlinePragma p -- See Note [Pragma source text]
      , inl_inline :: InlineSpec p    -- See Note [inl_inline and inl_act] in GHC.Hs.InlinePragma

      , inl_sat    :: Maybe Arity    -- Just n <=> Inline only when applied to n
                                     --            explicit (non-type, non-dictionary) args
                                     --   That is, inl_sat describes the number of *source-code*
                                     --   arguments the thing must be applied to.  We add on the
                                     --   number of implicit, dictionary arguments when making
                                     --   the Unfolding, and don't look at inl_sat further

      , inl_act    :: Activation p    -- Says during which phases inlining is allowed
                                      -- See Note [inl_inline and inl_act] in GHC.Hs.InlinePragma

      , inl_rule   :: RuleMatchInfo   -- Should the function be treated like a constructor?
    } 
  | XCInlinePragma (XXCInlinePragma p)


-- | Inline Specification
data InlineSpec p   -- What the user's INLINE pragma looked like
  = Inline    (XInline    p)       -- User wrote INLINE
  | Inlinable (XInlinable p)       -- User wrote INLINABLE
  | NoInline  (XNoInline  p)       -- User wrote NOINLINE
  | Opaque    (XOpaque    p)       -- User wrote OPAQUE
                               -- Each of the above keywords is accompanied with
                               -- a string of type SourceText written by the user
  | NoUserInlinePrag (XNoUserInlinePrag p) 
                     -- User did not write any of INLINE/INLINABLE/NOINLINE
                     -- e.g. in `defaultInlinePragma` or when created by CSE
  | XInlineSpec (XXInlineSpec p)

-- | Activation
data Activation p
  = AlwaysActive (XAlwaysActive p)
  | ActiveBefore (XActiveBefore p) PhaseNum  -- Active only *strictly before* this phase
  | ActiveAfter  (XActiveAfter  p) PhaseNum  -- Active in this phase and later
  | FinalActive  (XFinalActive  p)           -- Active in final phase only
  | NeverActive  (XNeverActive  p)
  | XActivation  (XXActivation  p)

-- | Phase Number
type PhaseNum = Int  -- Compilation phase
                     -- Phases decrease towards zero
                     -- Zero is the last phase

-- | Rule Match Information
data RuleMatchInfo = ConLike -- See Note [CONLIKE pragma] in GHC.Hs.InlinePragma
                   | FunLike
                   deriving( Eq, Data, Show )
  -- Show needed for GHC.Parser.Lexer

isConLike :: RuleMatchInfo -> Bool
isConLike ConLike = True
isConLike _       = False

isFunLike :: RuleMatchInfo -> Bool
isFunLike FunLike = True
isFunLike _       = False


