
-- The Summoner is a demand-driven inliner.
--   We give it the name of a function we want summoned, and it will inline
--   everything it can find into it.
--
--   The summoner ignores GHC generated inliner heuristics (UnfoldingGuidance)
--   as well as NOINLINE pragmas for bindings in the module being compiled.
--
--   It does respect loop breaker markers, as we can't summon into recursive
--   functions indefinately. It also respects INLINE [N] phase numbers,
--   because rewrite rules depend on these to fire.
--

module DPH.Pass.Summon
        (passSummon)
where
import DPH.Core.Pretty
import HscTypes
import CoreSyn
import CoreMonad
import Avail
import Data.Maybe
import Data.Set                 (Set)
import qualified UniqDFM        as UDFM
import qualified Data.Set       as Set
import Control.Monad
import Debug.Trace

-- Pass -----------------------------------------------------------------------
passSummon :: ModGuts -> CoreM ModGuts
passSummon guts
 = do   let tops        = mg_binds guts

        -- Get the names of the vectorised versions of all exported bindings.
        let nsExported  = [ n | Avail n <- mg_exports guts]
        let nsExported_vect
                = catMaybes
                $ map (UDFM.lookupUDFM (vectInfoVar $ mg_vect_info guts))
                $ nsExported

        -- Summon all of the vectorised things.
        let summonMe
                = Set.fromList
                $ map snd
                $ nsExported_vect

        tops'   <- mapM (summonTop summonMe tops) tops
        return  $ guts { mg_binds = tops'}


-- Top ------------------------------------------------------------------------
-- | If some `CoreBind` is in the set, then summon all its parts.
summonTop
        :: Set CoreBndr -- ^ Summon into bindings with these binders.
        -> [CoreBind]   -- ^ All the top-level bindings for this module.
        -> CoreBind     -- ^ Binding to inspect
        -> CoreM CoreBind

summonTop bsSet tops bind
 = case bind of
        NonRec b x
         -> do  (b', x')        <- goSummon (b, x)
                return $ NonRec b' x'

        Rec bxs
         -> do  bxs'            <- mapM goSummon bxs
                return $ Rec bxs'
 where
        goSummon (b, x)
         | Set.member b bsSet   = summon tops (b, x)
         | otherwise            = return (b, x)


-- Summon ---------------------------------------------------------------------
-- | Inline everything we can find into this binding.
summon  :: [CoreBind]            -- ^ All the top-level bindings for this module.
        -> (CoreBndr, Expr CoreBndr)   -- ^ The binding to work on.
        -> CoreM (CoreBndr, Expr CoreBndr)
summon tops (b, xx)
 = trace (renderIndent $ text "summoning " <> ppr b)
 $ do   xx'     <- summonX tops xx
        return  (b, xx')


-- | Summon into an expression.
summonX :: [CoreBind]
        -> Expr CoreBndr
        -> CoreM (Expr CoreBndr)
summonX tops xx
 = let down     = summonX tops
   in case xx of
        Var n
         -> trace (renderIndent $ text "look at " <> ppr n)
         $  case lookupBind tops n of
                Nothing -> return xx
                Just x' -> summonX tops x'

        Lit{}           -> return xx
        App x arg       -> liftM2 App (down x)  (down arg)
        Lam b   x       -> liftM2 Lam (return b)         (down x)
        Let bnd x       -> liftM2 Let (summonB tops bnd) (down x)

        Case x b t alts -> liftM4 Case  (down x)
                                (return b) (return t)
                                (mapM (summonA tops) alts)

        Cast x co       -> liftM2 Cast  (down x)   (return co)
        Tick t x        -> liftM2 Tick  (return t) (down x)
        Type t          -> return xx
        Coercion co     -> return xx


-- | Summon into an alternative.
summonA :: [CoreBind]
        -> (AltCon, [CoreBndr], Expr CoreBndr)
        -> CoreM (AltCon, [CoreBndr], Expr CoreBndr)

summonA tops (con, bs, x)
 = do   x'      <- summonX tops x
        return  $ (con, bs, x')


-- | Summon into a let-binding.
summonB :: [CoreBind]
        -> Bind CoreBndr
        -> CoreM (Bind CoreBndr)

summonB tops bb
 = case bb of
        NonRec b x
         -> liftM2 NonRec (return b) (summonX tops x)

        Rec bxs
         -> do  let (bs, xs)    = unzip bxs
                xs'             <- mapM (summonX tops) xs
                return          $ Rec $ zip bs xs


lookupBind
        :: [CoreBind]
        -> CoreBndr
        -> Maybe (Expr CoreBndr)
lookupBind tops b
 = case tops of
        []      -> Nothing

        (NonRec b' x : _)
         | b == b'      -> Just x

        _ : ts          -> lookupBind ts b

