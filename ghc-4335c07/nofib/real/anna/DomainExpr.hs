	
-- ==========================================================--
-- === Domain expressions.                                ===--
-- ===                                      DomainExpr.hs ===--
-- ==========================================================--

module DomainExpr where
import BaseDefs
import Utils
import MyUtils

-- ==========================================================--
--
dxApplyDSubst_2 :: DExpr -> Domain

dxApplyDSubst_2 DXTwo               = Two
dxApplyDSubst_2 (DXVar _)           = Two
dxApplyDSubst_2 (DXLift1 [])        = Two  -- *** kludge *** --
dxApplyDSubst_2 (DXLift1 dxs)       = Lift1 (map dxApplyDSubst_2 dxs)
dxApplyDSubst_2 (DXLift2 [])        = Lift1 [Two]
                                      -- ** MEGA HACK ** --
                                      -- panic "dxApplyDSubst_2"
dxApplyDSubst_2 (DXLift2 dxs)       = Lift2 (map dxApplyDSubst_2 dxs)
dxApplyDSubst_2 (DXFunc dxs dxt)    = Func (map dxApplyDSubst_2 dxs)
                                           (dxApplyDSubst_2 dxt)


-- ==========================================================--
--
dxApplyDSubst :: DSubst -> DExpr -> Domain

dxApplyDSubst rho DXTwo = Two
dxApplyDSubst rho (DXVar alpha)      = utSureLookup rho "dxApplySubst" alpha
dxApplyDSubst rho (DXLift1 [])       = Two  -- *** kludge *** --
dxApplyDSubst rho (DXLift1 dxs)      = Lift1 (map (dxApplyDSubst rho) dxs)
dxApplyDSubst rho (DXLift2 [])       = Lift1 [Two]
                                       -- ** MEGA HACK ** --
                                       -- panic "dxApplyDSubst"
dxApplyDSubst rho (DXLift2 dxs)      = Lift2 (map (dxApplyDSubst rho) dxs)
dxApplyDSubst rho (DXFunc dxs dxt)   = Func (map (dxApplyDSubst rho) dxs)
                                            (dxApplyDSubst rho dxt)


-- ==========================================================--
--
dxNormaliseDExpr :: DExpr -> DExpr

dxNormaliseDExpr (DXFunc dss (DXFunc dss2 dt))
   = dxNormaliseDExpr (DXFunc (dss++dss2) dt)
dxNormaliseDExpr (DXFunc dss dt)
   = DXFunc (map dxNormaliseDExpr dss) (dxNormaliseDExpr dt)

dxNormaliseDExpr DXTwo           = DXTwo
dxNormaliseDExpr (DXLift1 dxs)   = DXLift1 (map dxNormaliseDExpr dxs)
dxNormaliseDExpr (DXLift2 dxs)   = DXLift2 (map dxNormaliseDExpr dxs)
dxNormaliseDExpr (DXVar v)       = DXVar v


-- ==========================================================--
--
dxContainsFnSpace :: DExpr -> Bool

dxContainsFnSpace DXTwo           = False
dxContainsFnSpace (DXLift1 dxs)   = myAny dxContainsFnSpace dxs
dxContainsFnSpace (DXLift2 dxs)   = myAny dxContainsFnSpace dxs
dxContainsFnSpace (DXFunc _ _)    = True
dxContainsFnSpace (DXVar _)       = False


-- ==========================================================--
--
dxContainsSubsidiaryFnSpace :: DExpr -> Bool

dxContainsSubsidiaryFnSpace DXTwo 
   = False

dxContainsSubsidiaryFnSpace (DXLift1 dxs) 
   = myAny dxContainsFnSpace dxs

dxContainsSubsidiaryFnSpace (DXLift2 dxs) 
   = myAny dxContainsFnSpace dxs

dxContainsSubsidiaryFnSpace (DXFunc dxss dxt)
   = myAny dxContainsFnSpace dxss || dxContainsFnSpace dxt

dxContainsSubsidiaryFnSpace (DXVar _)
   = False


-- ==========================================================--
--        big       small
dxDiff :: Domain -> Domain -> (DExpr, DSubst)

dxDiff db ds
   = case
        doStatefulOp2 dxDiff_aux (fromEnum 'a', []) ds db
     of
        (dexpr, (num, dsubst)) -> (dexpr, dsubst)


dxDiff_aux Two Two
   = returnS DXTwo

dxDiff_aux Two not_two
   = fetchS                                  `thenS`  ( \ (n, sub) ->
     assignS (n+1, ([toEnum n], not_two):sub)   `thenS`  ( \ () ->
     returnS (DXVar [toEnum n])
     ))

dxDiff_aux (Lift1 ds1) (Lift1 ds2)
   = dxDiff_list ds1 ds2              `thenS` ( \new_ds1_ds2 ->
     returnS (DXLift1 new_ds1_ds2)
     )

dxDiff_aux (Lift2 ds1) (Lift2 ds2)
   = dxDiff_list ds1 ds2              `thenS` ( \new_ds1_ds2 ->
     returnS (DXLift2 new_ds1_ds2)
     )

dxDiff_aux (Func dss1 dt1) (Func dss2 dt2)
   = dxDiff_list dss1 dss2            `thenS` ( \new_dss1_dss2 ->
     dxDiff_aux dt1 dt2               `thenS` ( \new_dt1_dt2 ->
     returnS (DXFunc new_dss1_dss2 new_dt1_dt2)
     ))

dxDiff_aux other1 other2
   = panic "dxDiff_aux"


dxDiff_list [] []
   = returnS []

dxDiff_list (a:as) (b:bs)
   = dxDiff_aux a b                   `thenS`  ( \new_a_b ->
     dxDiff_list as bs                `thenS`  ( \new_as_bs ->
     returnS (new_a_b : new_as_bs)  
     ))

dxDiff_list other1 other2
   = panic "dxDiff_list: unequal lists"


-- ==========================================================--
-- === end                                  DomainExpr.hs ===--
-- ==========================================================--
