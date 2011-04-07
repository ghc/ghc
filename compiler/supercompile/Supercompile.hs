module Supercompile (supercompile) where

import Supercompile.Core.FreeVars
import Supercompile.Core.Renaming
import Supercompile.Core.Size
import Supercompile.Core.Syntax

-- import CoreSyn
-- 
-- supercompile :: CoreExpr -> CoreExpr
-- supercompile e = traceRender ("all input FVs", input_fvs) $ second (fVedTermToTerm . if pRETTIFY then prettify else id) $ runScpM $ liftM snd $ sc (mkHistory (extra wQO)) S.empty (wARP_FACTOR * input_size, state)
--   where input_fvs = annedTermFreeVars anned_e
--         input_size = annedSize anned_e
--         state = normalise ((bLOAT_FACTOR - 1) * input_size, Heap (M.fromDistinctAscList anned_h_kvs) reduceIdSupply, [], (mkIdentityRenaming $ S.toAscList input_fvs, anned_e))
--         
--         (tag_ids, anned_h_kvs) = mapAccumL (\tag_ids x' -> let (tag_ids', i) = stepIdSupply tag_ids in (tag_ids', (x', environmentallyBound (mkTag (hashedId i))))) tagIdSupply (S.toList input_fvs)
--         anned_e = toAnnedTerm tag_ids e