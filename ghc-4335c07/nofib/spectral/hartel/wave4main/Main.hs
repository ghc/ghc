module Main (main) -- wave4main
where {
    import System.Environment (getArgs);
--partain: import Fast2haskell;
#include "../Fast2haskell.hs"
    strict_show_i::Int -> [Char];
    strict_show_i x=miraseq x (show x);
    strict_show_d::Double -> [Char];
    strict_show_d x=miraseq x (show x);

    f_benchmark_main a_n=(++) (f_sumcode (f_output_print (f_solution a_n))) "\n";
    f_sumcode::[Char] -> [Char];
    f_sumcode a_xs=
        let { 
            f_sumcode' [] a_sum a_n=(++) (strict_show_i (((+) :: (Int -> Int -> Int)) a_sum a_n)) ((:) '/' (strict_show_i a_n));
            f_sumcode' (a_x:a_xs) a_sum a_n=f_sumcode' a_xs (((+) :: (Int -> Int -> Int)) a_sum (fromEnum a_x)) (((+) :: (Int -> Int -> Int)) a_n (1 :: Int))
         } in  f_sumcode' a_xs (0 :: Int) (0 :: Int);
type 
    T_matrix t1=Array_type (Array_type t1);
    f_descr_print::Descr_type -> [Char];
    f_descr_print a_d=
        let { 
            r_low=lowbound a_d;
            r_up=upbound a_d
         } in  (++) "[" ((++) (strict_show_i r_low) ((++) ".." ((++) (strict_show_i r_up) "]")));
    f_array_print::(t1 -> [Char]) -> Char -> (Array_type t1) -> [Char];
    f_array_print a_pr a_sep a_arr=(++) (f_descr_print (bounds a_arr)) (f_concat [(:) a_sep (a_pr a_n)|a_n<-
        elems a_arr]);
    f_matrix_print::(t1 -> [Char]) -> Char -> (T_matrix t1) -> [Char];
    f_matrix_print a_pr a_sep a_mat=(++) (f_descr_print (bounds a_mat)) (f_concat [(:) a_sep (f_array_print a_pr ',' a_a)|a_a<-
        elems a_mat]);
    f_tabulate2::(Int -> Int -> t1) -> Descr_type -> Descr_type -> T_matrix t1;
    f_tabulate2 a_f a_di a_dj=
        let { 
            f_tabhulp a_f a_dj a_i=tabulate (a_f a_i) a_dj
         } in  tabulate (f_tabhulp a_f a_dj) a_di;
    f_getdescr2::(T_matrix t1) -> (Descr_type,Descr_type);
    f_getdescr2 a_arr=
        let { 
            r_dx=bounds a_arr;
            r_dy=bounds ((!) a_arr (lowbound r_dx))
         } in  (r_dx,r_dy);
    f_subscript2::(T_matrix t1) -> Int -> Int -> t1;
    f_subscript2 a_a a_i a_j=(!) ((!) a_a a_i) a_j;
    f_transpose2::(T_matrix t1) -> T_matrix t1;
    f_transpose2 a_arr=
        let { 
            (r_dx,r_dy)=f_getdescr2 a_arr;
            f_subhulp a_arr a_j a_i=f_subscript2 a_arr a_i a_j
         } in  f_tabulate2 (f_subhulp a_arr) r_dy r_dx;
    f_updaterange::(T_matrix t1) -> (T_matrix t1) -> T_matrix t1;
    f_updaterange a_a a_b=
        let { 
            (r_dax,r_day)=f_getdescr2 a_a
         } in  f_tabulate2 (f_updatehulp a_a a_b) r_dax r_day;
    f_updatehulp::(T_matrix t1) -> (T_matrix t1) -> Int -> Int -> t1;
    f_updatehulp a_a a_b a_i a_j=
        let { 
            r_in_bx=f_indexindescr a_i r_dbx;
            r_in_by=f_indexindescr a_j r_dby;
            (r_dbx,r_dby)=f_getdescr2 a_b
         } in  
            if (
                if r_in_bx
                then r_in_by
                else 
                    False)
            then (f_subscript2 a_b a_i a_j)
            else 
                (f_subscript2 a_a a_i a_j);
    f_getleftcol::(T_matrix t1) -> Array_type t1;
    f_getleftcol a_arr=f_getfirstel a_arr;
    f_getrightcol::(T_matrix t1) -> Array_type t1;
    f_getrightcol a_arr=f_getlastel a_arr;
    f_getbottomrow::(T_matrix t1) -> Array_type t1;
    f_getbottomrow a_arr=
        let { 
            f_getbottomhulp a_arr a_i=f_getfirstel ((!) a_arr a_i)
         } in  tabulate (f_getbottomhulp a_arr) (bounds a_arr);
    f_gettoprow::(T_matrix t1) -> Array_type t1;
    f_gettoprow a_arr=
        let { 
            f_gettophulp a_arr a_i=f_getlastel ((!) a_arr a_i)
         } in  tabulate (f_gettophulp a_arr) (bounds a_arr);
    f_prependcol::(T_matrix t1) -> (Array_type t1) -> T_matrix t1;
    f_prependcol a_arr a_col=f_prependel a_arr a_col;
    f_appendcol::(T_matrix t1) -> (Array_type t1) -> T_matrix t1;
    f_appendcol a_arr a_col=f_appendel a_arr a_col;
    f_prependrow::(T_matrix t1) -> (Array_type t1) -> T_matrix t1;
    f_prependrow a_arr a_row=
        let { 
            f_prependhulp a_arr a_row a_i=f_prependel ((!) a_arr a_i) ((!) a_row a_i)
         } in  tabulate (f_prependhulp a_arr a_row) (bounds a_arr);
    f_appendrow::(T_matrix t1) -> (Array_type t1) -> T_matrix t1;
    f_appendrow a_arr a_row=
        let { 
            f_appendhulp a_arr a_row a_i=f_appendel ((!) a_arr a_i) ((!) a_row a_i)
         } in  tabulate (f_appendhulp a_arr a_row) (bounds a_arr);
    f_indexindescr::Int -> Descr_type -> Bool;
    f_indexindescr a_i a_d=
        if (((>=) :: (Int -> Int -> Bool)) a_i (lowbound a_d))
        then (((<=) :: (Int -> Int -> Bool)) a_i (upbound a_d))
        else 
            False;
    f_getfirstel::(Array_type t1) -> t1;
    f_getfirstel a_arr=(!) a_arr (lowbound (bounds a_arr));
    f_getlastel::(Array_type t1) -> t1;
    f_getlastel a_arr=(!) a_arr (upbound (bounds a_arr));
    f_prependel::(Array_type t1) -> t1 -> Array_type t1;
    f_prependel a_ar a_x=
        let { 
            r_lu=bounds a_ar;
            r_l=lowbound r_lu;
            r_u=upbound r_lu;
            f_generate a_i=
                if (((<) :: (Int -> Int -> Bool)) a_i r_l)
                then a_x
                else 
                    ((!) a_ar a_i)
         } in  tabulate f_generate (descr (((-) :: (Int -> Int -> Int)) r_l (1 :: Int)) r_u);
    f_appendel::(Array_type t1) -> t1 -> Array_type t1;
    f_appendel a_ar a_x=
        let { 
            r_lu=bounds a_ar;
            r_l=lowbound r_lu;
            r_u=upbound r_lu;
            f_generate a_i=
                if (((>) :: (Int -> Int -> Bool)) a_i r_u)
                then a_x
                else 
                    ((!) a_ar a_i)
         } in  tabulate f_generate (descr r_l (((+) :: (Int -> Int -> Int)) r_u (1 :: Int)));
    c_imax,c_jmax,c_imax1,c_jmax1,c_imid,c_imid1,c_jmid,c_jmid1::Int;
    c_imax=(7 :: Int);
    c_jmax=(7 :: Int);
    c_imax1=((+) :: (Int -> Int -> Int)) c_imax (1 :: Int);
    c_jmax1=((+) :: (Int -> Int -> Int)) c_jmax (1 :: Int);
    c_imid=((-) :: (Int -> Int -> Int)) (((quot) :: (Int -> Int -> Int)) c_imax1 (2 :: Int)) (1 :: Int);
    c_imid1=((+) :: (Int -> Int -> Int)) c_imid (1 :: Int);
    c_jmid=((-) :: (Int -> Int -> Int)) (((quot) :: (Int -> Int -> Int)) c_jmax1 (2 :: Int)) (1 :: Int);
    c_jmid1=((+) :: (Int -> Int -> Int)) c_jmid (1 :: Int);
    c_deltax,c_deltay,c_deltat,c_fcr,c_gam,c_psi,c_gr,c_lbd,c_vwn::Double;
    c_deltax=(10000.0 :: Double);
    c_deltay=(10000.0 :: Double);
    c_deltat=(800.000 :: Double);
    c_fcr=(0.000125000 :: Double);
    c_gam=(3.20000e-06 :: Double);
    c_psi=(0.00000 :: Double);
    c_gr=(9.80000 :: Double);
    c_lbd=(0.00240000 :: Double);
    c_vwn=(0.00000 :: Double);
type 
    T_mat=T_matrix Double;
type 
    T_col=Array_type Double;
type 
    T_row=Array_type Double;
type 
    T_triplet=(T_mat,T_mat,T_mat);
    f_u0::Int -> Int -> Double;
    f_u0 a_i a_j=(0.00000 :: Double);
    f_v0::Int -> Int -> Double;
    f_v0 a_i a_j=(0.00000 :: Double);
    f_h0::Int -> Int -> Double;
    f_h0 a_i a_j=((/) :: (Double -> Double -> Double)) (fromIntegral (((*) :: (Int -> Int -> Int)) (3 :: Int) a_i)) (fromIntegral c_imax);
    f_d::Int -> Int -> Double;
    f_d a_i a_j=(30.0000 :: Double);
    c_cux,c_cuy,c_ccr,c_cfr,c_windx,c_windy,c_chx,c_chy::Double;
    c_cux=((*) :: (Double -> Double -> Double)) c_gr (((/) :: (Double -> Double -> Double)) c_deltat (((*) :: (Double -> Double -> Double)) (2.00000 :: Double) c_deltax));
    c_cuy=((*) :: (Double -> Double -> Double)) c_gr (((/) :: (Double -> Double -> Double)) c_deltat (((*) :: (Double -> Double -> Double)) (2.00000 :: Double) c_deltay));
    c_ccr=((*) :: (Double -> Double -> Double)) c_fcr (((/) :: (Double -> Double -> Double)) c_deltat (4.00000 :: Double));
    c_cfr=((*) :: (Double -> Double -> Double)) (2.00000 :: Double) c_deltat;
    c_windx=((*) :: (Double -> Double -> Double)) c_gam (((*) :: (Double -> Double -> Double)) c_vwn (((*) :: (Double -> Double -> Double)) c_vwn (((cos) :: (Double -> Double)) c_psi)));
    c_windy=((*) :: (Double -> Double -> Double)) c_gam (((*) :: (Double -> Double -> Double)) c_vwn (((*) :: (Double -> Double -> Double)) c_vwn (((sin) :: (Double -> Double)) c_psi)));
    c_chx=((/) :: (Double -> Double -> Double)) c_deltat (((*) :: (Double -> Double -> Double)) (4.00000 :: Double) c_deltax);
    c_chy=((/) :: (Double -> Double -> Double)) c_deltat (((*) :: (Double -> Double -> Double)) (4.00000 :: Double) c_deltay);
    f_updu::T_mat -> T_mat -> T_mat -> Int -> Int -> Double;
    f_updu a_u a_v a_h a_i a_j=
        let { 
            r_height=((*) :: (Double -> Double -> Double)) c_cux (((-) :: (Double -> Double -> Double)) (f_subscript2 a_h a_i a_j) (f_subscript2 a_h (((-) :: (Int -> Int -> Int)) a_i (1 :: Int)) a_j));
            r_coriolis=
                let { 
                    r_v1=f_subscript2 a_v (((-) :: (Int -> Int -> Int)) a_i (1 :: Int)) a_j;
                    r_v2=f_subscript2 a_v (((-) :: (Int -> Int -> Int)) a_i (1 :: Int)) (((+) :: (Int -> Int -> Int)) a_j (1 :: Int));
                    r_v3=f_subscript2 a_v a_i a_j;
                    r_v4=f_subscript2 a_v a_i (((+) :: (Int -> Int -> Int)) a_j (1 :: Int))
                 } in  ((*) :: (Double -> Double -> Double)) c_ccr (((+) :: (Double -> Double -> Double)) r_v1 (((+) :: (Double -> Double -> Double)) r_v2 (((+) :: (Double -> Double -> Double)) r_v3 r_v4)));
            r_friction=((*) :: (Double -> Double -> Double)) c_cfr (((/) :: (Double -> Double -> Double)) (((-) :: (Double -> Double -> Double)) r_bodem c_windx) (((+) :: (Double -> Double -> Double)) (f_d a_i a_j) (f_d a_i 
                (((+) :: (Int -> Int -> Int)) a_j (1 :: Int)))));
            r_bodem=((*) :: (Double -> Double -> Double)) c_lbd (f_subscript2 a_u a_i a_j)
         } in  
            if (((==) :: (Int -> Int -> Bool)) a_i (0 :: Int))
            then (0.00000 :: Double)
            else 
            if (((==) :: (Int -> Int -> Bool)) a_i c_imax1)
            then (0.00000 :: Double)
            else 
                (((+) :: (Double -> Double -> Double)) (((-) :: (Double -> Double -> Double)) (f_subscript2 a_u a_i a_j) r_height) (((-) :: (Double -> Double -> Double)) r_coriolis r_friction));
    f_updv::T_mat -> T_mat -> T_mat -> Int -> Int -> Double;
    f_updv a_u a_v a_h a_i a_j=
        let { 
            r_height=((*) :: (Double -> Double -> Double)) c_cuy (((-) :: (Double -> Double -> Double)) (f_subscript2 a_h a_i a_j) (f_subscript2 a_h a_i (((-) :: (Int -> Int -> Int)) a_j (1 :: Int))));
            r_coriolis=
                let { 
                    r_u1=f_subscript2 a_u a_i (((-) :: (Int -> Int -> Int)) a_j (1 :: Int));
                    r_u2=f_subscript2 a_u (((+) :: (Int -> Int -> Int)) a_i (1 :: Int)) (((-) :: (Int -> Int -> Int)) a_j (1 :: Int));
                    r_u3=f_subscript2 a_u a_i a_j;
                    r_u4=f_subscript2 a_u (((+) :: (Int -> Int -> Int)) a_i (1 :: Int)) a_j
                 } in  ((*) :: (Double -> Double -> Double)) c_ccr (((+) :: (Double -> Double -> Double)) r_u1 (((+) :: (Double -> Double -> Double)) r_u2 (((+) :: (Double -> Double -> Double)) r_u3 r_u4)));
            r_friction=((*) :: (Double -> Double -> Double)) c_cfr (((/) :: (Double -> Double -> Double)) (((-) :: (Double -> Double -> Double)) r_bodem c_windy) (((+) :: (Double -> Double -> Double)) (f_d a_i a_j) (f_d 
                (((+) :: (Int -> Int -> Int)) a_i (1 :: Int)) a_j)));
            r_bodem=((*) :: (Double -> Double -> Double)) c_lbd (f_subscript2 a_v a_i a_j)
         } in  
            if (((==) :: (Int -> Int -> Bool)) a_j (0 :: Int))
            then (0.00000 :: Double)
            else 
            if (((==) :: (Int -> Int -> Bool)) a_j c_jmax1)
            then (0.00000 :: Double)
            else 
                (((-) :: (Double -> Double -> Double)) (((-) :: (Double -> Double -> Double)) (f_subscript2 a_v a_i a_j) r_height) (((+) :: (Double -> Double -> Double)) r_coriolis r_friction));
    f_updh::T_mat -> T_mat -> T_mat -> Int -> Int -> Double;
    f_updh a_u a_v a_h a_i a_j=
        let { 
            r_d1=((*) :: (Double -> Double -> Double)) (((+) :: (Double -> Double -> Double)) (f_d (((+) :: (Int -> Int -> Int)) a_i (1 :: Int)) a_j) (f_d (((+) :: (Int -> Int -> Int)) a_i (1 :: Int)) 
                (((+) :: (Int -> Int -> Int)) a_j (1 :: Int)))) (f_subscript2 a_u (((+) :: (Int -> Int -> Int)) a_i (1 :: Int)) a_j);
            r_d2=((*) :: (Double -> Double -> Double)) (((+) :: (Double -> Double -> Double)) (f_d a_i a_j) (f_d a_i (((+) :: (Int -> Int -> Int)) a_j (1 :: Int)))) (f_subscript2 a_u a_i a_j);
            r_d3=((*) :: (Double -> Double -> Double)) (((+) :: (Double -> Double -> Double)) (f_d a_i (((+) :: (Int -> Int -> Int)) a_j (1 :: Int))) (f_d (((+) :: (Int -> Int -> Int)) a_i (1 :: Int)) 
                (((+) :: (Int -> Int -> Int)) a_j (1 :: Int)))) (f_subscript2 a_v a_i (((+) :: (Int -> Int -> Int)) a_j (1 :: Int)));
            r_d4=((*) :: (Double -> Double -> Double)) (((+) :: (Double -> Double -> Double)) (f_d a_i a_j) (f_d (((+) :: (Int -> Int -> Int)) a_i (1 :: Int)) a_j)) (f_subscript2 a_v a_i a_j)
         } in  ((-) :: (Double -> Double -> Double)) (((-) :: (Double -> Double -> Double)) (f_subscript2 a_h a_i a_j) (((*) :: (Double -> Double -> Double)) c_chx (((-) :: (Double -> Double -> Double)) r_d1 r_d2))) (((*) :: (Double -> Double -> Double)) c_chy 
            (((-) :: (Double -> Double -> Double)) r_d3 r_d4));
    f_printall::[T_triplet] -> [Char];
    f_printall a_trips=
        let { 
            (r_us,r_vs,r_hs)=f_unzip3 a_trips
         } in  f_printtrip (f_join r_us,f_join r_vs,f_join r_hs);
    f_printtrip::T_triplet -> [Char];
    f_printtrip (a_u,a_v,a_h)=
        let { 
            r_us=f_matrix_print (f_showfix (3 :: Int)) '\o012' (f_transpose2 a_u);
            r_vs=f_matrix_print (f_showfix (3 :: Int)) '\o012' (f_transpose2 a_v);
            r_hs=f_matrix_print (f_showfix (3 :: Int)) '\o012' (f_transpose2 a_h)
         } in  f_concat ((:) r_us ((:) ((:) '\o012' []) ((:) r_vs ((:) 
            ((:) '\o012' []) ((:) r_hs ((:) ((:) '\o012' []) []))))));
    f_showfix::Int -> Double -> [Char];
    f_showfix a_w a_x=
        let { 
            r_sign=
                if (((<) :: (Double -> Double -> Bool)) a_x (0.00000 :: Double))
                then '-'
                else 
                    ' ';
            r_i=floor (entier (((+) :: (Double -> Double -> Double)) (0.500000 :: Double) (f_abs (((*) :: (Double -> Double -> Double)) a_x (100.000 :: Double)))));
            r_d3_c=toEnum (((+) :: (Int -> Int -> Int)) (fromEnum '0') (((rem) :: (Int -> Int -> Int)) r_i (10 :: Int)));
            r_d2_c=toEnum (((+) :: (Int -> Int -> Int)) (fromEnum '0') (((rem) :: (Int -> Int -> Int)) (((quot) :: (Int -> Int -> Int)) r_i (10 :: Int)) (10 :: Int)));
            r_d1_c=toEnum (((+) :: (Int -> Int -> Int)) (fromEnum '0') (((rem) :: (Int -> Int -> Int)) (((quot) :: (Int -> Int -> Int)) r_i (100 :: Int)) (10 :: Int)))
         } in  
            if (((>) :: (Int -> Int -> Bool)) r_i (999 :: Int))
            then "*****"
            else 
                ((:) r_sign ((:) r_d1_c ((:) '.' ((:) r_d2_c ((:) r_d3_c [])))));
    f_join::[T_mat] -> T_mat;
    f_join a_ranges=
        let { 
            r_arr=f_tabulate2 f_zero2 (descr (0 :: Int) c_imax) (descr (0 :: Int) c_jmax)
         } in  f_foldl f_updaterange r_arr a_ranges;
    f_zero2::Int -> Int -> Double;
    f_zero2 a_i a_j=(0.00000 :: Double);
    f_unzip3::[(t1,t2,t3)] -> ([t1],[t2],[t3]);
    f_unzip3 []=([],[],[]);
    f_unzip3 ((a_u,a_v,a_h):a_ts)=
        let { 
            (r_us,r_vs,r_hs)=f_unzip3 a_ts
         } in  ((:) a_u r_us,(:) a_v r_vs,(:) a_h r_hs);
type 
    T_double_matrix=T_matrix Double;
type 
    T_double_array=Array_type Double;
type 
    T_double_matrix_triple=(T_double_matrix,T_double_matrix,T_double_matrix);
type 
    T_double_array_tuple=(T_double_array,T_double_array);
type 
    T_double_matrix_triple_pair=(T_double_matrix_triple,T_double_matrix_triple);
    f_matrix_first_col a_m=f_getleftcol a_m;
    f_matrix_last_col a_m=f_getrightcol a_m;
    f_matrix_tab a_f (a_dx,a_dy)=f_tabulate2 a_f a_dx a_dy;
    f_matrix_append_col a_m a_c=f_appendcol a_m a_c;
    f_matrix_prepend_col a_m a_c=f_prependcol a_m a_c;
    f_matrix_sub a_m a_i a_j=f_subscript2 a_m a_i a_j;
    f_solution::Int -> T_double_matrix_triple_pair;
    f_solution a_n=f_prog c_mf0 c_mg0 (f_first_borders c_mg0) a_n;
    f_prog::T_double_matrix_triple -> T_double_matrix_triple -> T_double_array_tuple -> Int -> T_double_matrix_triple_pair;
    f_prog a_mfh a_mgh a_mghds 0=(a_mfh,a_mgh);
    f_prog a_mfh a_mgh a_mghds a_n=
        let { 
            r_mfh'=f_fvh r_mfu;
            r_mghds'=f_first_borders r_mgh';
            r_mgh'=f_gvh r_mgu r_mfulst;
            r_mfulst=f_last_borders r_mfu;
            r_mfu=f_fu a_mfh a_mghds;
            r_mgu=f_gu a_mgh
         } in  f_prog r_mfh' r_mgh' r_mghds' (((-) :: (Int -> Int -> Int)) a_n (1 :: Int));
    c_mf0,c_mg0::T_double_matrix_triple;
    c_mf0=(c_ul0,c_vl0,c_hl0);
    c_mg0=(c_ur0,c_vr0,c_hr0);
    f_fvh::T_double_matrix_triple -> T_double_matrix_triple;
    f_fvh a_mfu=f_fh (f_fv a_mfu);
    f_gvh::T_double_matrix_triple -> T_double_array -> T_double_matrix_triple;
    f_gvh a_mgu a_mfulst=f_gh (f_gv a_mgu a_mfulst) a_mfulst;
    f_first_borders::T_double_matrix_triple -> T_double_array_tuple;
    f_first_borders (a_u,a_v,a_h)=(f_matrix_first_col a_v,f_matrix_first_col a_h);
    f_last_borders::T_double_matrix_triple -> T_double_array;
    f_last_borders (a_u,a_v,a_h)=f_matrix_last_col a_u;
    f_fu::T_double_matrix_triple -> T_double_array_tuple -> T_double_matrix_triple;
    f_fu (a_u,a_v,a_h) (a_vc,a_hc)=
        let { 
            r_u1=f_matrix_tab (f_updu a_u (f_matrix_append_col a_v a_vc) (f_matrix_append_col a_h a_hc)) c_dul
         } in  (r_u1,a_v,a_h);
    f_fv::T_double_matrix_triple -> T_double_matrix_triple;
    f_fv (a_u,a_v,a_h)=
        let { 
            r_v1=f_matrix_tab (f_updv a_u a_v a_h) c_dvl
         } in  (a_u,r_v1,a_h);
    f_fh::T_double_matrix_triple -> T_double_matrix_triple;
    f_fh (a_u,a_v,a_h)=
        let { 
            r_h1=f_matrix_tab (f_updh a_u a_v a_h) c_dhl
         } in  (a_u,a_v,r_h1);
    f_gu::T_double_matrix_triple -> T_double_matrix_triple;
    f_gu (a_u,a_v,a_h)=
        let { 
            r_u1=f_matrix_tab (f_updu a_u a_v a_h) c_dur
         } in  (r_u1,a_v,a_h);
    f_gv::T_double_matrix_triple -> T_double_array -> T_double_matrix_triple;
    f_gv (a_u,a_v,a_h) a_uc=
        let { 
            r_v1=f_matrix_tab (f_updv (f_matrix_prepend_col a_u a_uc) a_v a_h) c_dvr
         } in  (a_u,r_v1,a_h);
    f_gh::T_double_matrix_triple -> T_double_array -> T_double_matrix_triple;
    f_gh (a_u,a_v,a_h) a_uc=
        let { 
            r_h1=f_matrix_tab (f_updh (f_matrix_prepend_col a_u a_uc) a_v a_h) c_dhr
         } in  (a_u,a_v,r_h1);
    c_k::Int;
    c_k=((quot) :: (Int -> Int -> Int)) c_imax1 (2 :: Int);
    c_dul,c_dvl,c_dhl,c_dur,c_dvr,c_dhr::(Descr_type,Descr_type);
    c_dul=(descr (0 :: Int) c_k,descr (0 :: Int) c_jmax);
    c_dvl=(descr (0 :: Int) (((-) :: (Int -> Int -> Int)) c_k (1 :: Int)),descr (0 :: Int) c_jmax1);
    c_dhl=(descr (0 :: Int) (((-) :: (Int -> Int -> Int)) c_k (1 :: Int)),descr (0 :: Int) c_jmax);
    c_dur=(descr (((+) :: (Int -> Int -> Int)) c_k (1 :: Int)) c_imax1,descr (0 :: Int) c_jmax);
    c_dvr=(descr c_k c_imax,descr (0 :: Int) c_jmax1);
    c_dhr=(descr c_k c_imax,descr (0 :: Int) c_jmax);
    c_ul0,c_vl0,c_hl0,c_ur0,c_vr0,c_hr0::T_double_matrix;
    c_ul0=f_matrix_tab f_u0 c_dul;
    c_vl0=f_matrix_tab f_v0 c_dvl;
    c_hl0=f_matrix_tab f_h0 c_dhl;
    c_ur0=f_matrix_tab f_u0 c_dur;
    c_vr0=f_matrix_tab f_v0 c_dvr;
    c_hr0=f_matrix_tab f_h0 c_dhr;
    f_output_print::T_double_matrix_triple_pair -> [Char];
    f_output_print ((a_lu,a_lv,a_lh),(a_ru,a_rv,a_rh))=f_concat [(++) (f_matrix_print (f_showfix (2 :: Int)) '\o012' a_m) "\n"|a_m<-(:) a_lu ((:) a_ru 
        ((:) a_lv ((:) a_rv ((:) a_lh ((:) a_rh [])))))];
    f_abs::Double -> Double;
    f_abs a_x=
        if (((<=) :: (Double -> Double -> Bool)) a_x (0.00000 :: Double))
        then (((negate) :: (Double -> Double)) a_x)
        else 
            a_x;
    f_and::[Bool] -> Bool;
    f_and a_xs=f_foldr (&&) True a_xs;
    f_cjustify::Int -> [Char] -> [Char];
    f_cjustify a_n a_s=
        let { 
            r_margin=((-) :: (Int -> Int -> Int)) a_n (length a_s);
            r_lmargin=((quot) :: (Int -> Int -> Int)) r_margin (2 :: Int);
            r_rmargin=((-) :: (Int -> Int -> Int)) r_margin r_lmargin
         } in  (++) (f_spaces r_lmargin) ((++) a_s (f_spaces r_rmargin));
    f_concat::[[t1]] -> [t1];
    f_concat a_xs=f_foldr (++) [] a_xs;
    f_const::t1 -> t2 -> t1;
    f_const a_x a_y=a_x;
    f_digit::Char -> Bool;
    f_digit a_x=
        if (((<=) :: (Int -> Int -> Bool)) (fromEnum '0') (fromEnum a_x))
        then (((<=) :: (Int -> Int -> Bool)) (fromEnum a_x) (fromEnum '9'))
        else 
            False;
    f_drop::Int -> [t1] -> [t1];
    f_drop 0 a_x=a_x;
    f_drop a_n (a_a:a_x)=f_drop (((-) :: (Int -> Int -> Int)) a_n (1 :: Int)) a_x;
    f_drop a_n a_x=[];
    f_dropwhile::(t1 -> Bool) -> [t1] -> [t1];
    f_dropwhile a_f []=[];
    f_dropwhile a_f (a_a:a_x)=
        if (a_f a_a)
        then (f_dropwhile a_f a_x)
        else 
            ((:) a_a a_x);
    c_e::Double;
    c_e=((exp) :: (Double -> Double)) (1.00000 :: Double);
    f_filter::(t1 -> Bool) -> [t1] -> [t1];
    f_filter a_f a_x=[a_a|a_a<-a_x,a_f a_a];
    f_foldl::(t1 -> t2 -> t1) -> t1 -> [t2] -> t1;
    f_foldl a_op a_r []=a_r;
    f_foldl a_op a_r (a_a:a_x)=
        let { 
            f_strict a_f a_x=miraseq a_x (a_f a_x)
         } in  f_foldl a_op (f_strict a_op a_r a_a) a_x;
    f_foldl1::(t1 -> t1 -> t1) -> [t1] -> t1;
    f_foldl1 a_op (a_a:a_x)=f_foldl a_op a_a a_x;
    f_foldr::(t1 -> t2 -> t2) -> t2 -> [t1] -> t2;
    f_foldr a_op a_r []=a_r;
    f_foldr a_op a_r (a_a:a_x)=a_op a_a (f_foldr a_op a_r a_x);
    f_foldr1::(t1 -> t1 -> t1) -> [t1] -> t1;
    f_foldr1 a_op (a_a:[])=a_a;
    f_foldr1 a_op (a_a:a_b:a_x)=a_op a_a (f_foldr1 a_op ((:) a_b a_x));
    f_fst::(t1,t2) -> t1;
    f_fst (a_a,a_b)=a_a;
    f_id::t1 -> t1;
    f_id a_x=a_x;
    f_index::[t1] -> [Int];
    f_index a_x=
        let { 
            f_f a_n []=[];
            f_f a_n (a_a:a_x)=(:) a_n (f_f (((+) :: (Int -> Int -> Int)) a_n (1 :: Int)) a_x)
         } in  f_f (0 :: Int) a_x;
    f_init::[t1] -> [t1];
    f_init (a_a:a_x)=
        if (null a_x)
        then []
        else 
            ((:) a_a (f_init a_x));
    f_iterate::(t1 -> t1) -> t1 -> [t1];
    f_iterate a_f a_x=(:) a_x (f_iterate a_f (a_f a_x));
    f_last::[t1] -> t1;
    f_last a_x=(!!) a_x (((-) :: (Int -> Int -> Int)) (length a_x) (1 :: Int));
    f_lay::[[Char]] -> [Char];
    f_lay []=[];
    f_lay (a_a:a_x)=(++) a_a ((++) "\n" (f_lay a_x));
    f_layn::[[Char]] -> [Char];
    f_layn a_x=
        let { 
            f_f a_n []=[];
            f_f a_n (a_a:a_x)=(++) (f_rjustify (4 :: Int) (strict_show_i a_n)) ((++) ") " ((++) a_a ((++) "\n" 
                (f_f (((+) :: (Int -> Int -> Int)) a_n (1 :: Int)) a_x))))
         } in  f_f (1 :: Int) a_x;
    f_letter::Char -> Bool;
    f_letter a_c=
        if (
            if (((<=) :: (Int -> Int -> Bool)) (fromEnum 'a') (fromEnum a_c))
            then (((<=) :: (Int -> Int -> Bool)) (fromEnum a_c) (fromEnum 'z'))
            else 
                False)
        then True
        else 
        if (((<=) :: (Int -> Int -> Bool)) (fromEnum 'A') (fromEnum a_c))
        then (((<=) :: (Int -> Int -> Bool)) (fromEnum a_c) (fromEnum 'Z'))
        else 
            False;
    f_limit::[Double] -> Double;
    f_limit (a_a:a_b:a_x)=
        if (((==) :: (Double -> Double -> Bool)) a_a a_b)
        then a_a
        else 
            (f_limit ((:) a_b a_x));
    f_lines::[Char] -> [[Char]];
    f_lines []=[];
    f_lines (a_a:a_x)=
        let { 
            r_xs=
                if (pair a_x)
                then (f_lines a_x)
                else 
                    ((:) [] [])
         } in  
            if (((==) :: (Int -> Int -> Bool)) (fromEnum a_a) (fromEnum '\o012'))
            then ((:) [] (f_lines a_x))
            else 
                ((:) ((:) a_a (head r_xs)) (tail r_xs));
    f_ljustify::Int -> [Char] -> [Char];
    f_ljustify a_n a_s=(++) a_s (f_spaces (((-) :: (Int -> Int -> Int)) a_n (length a_s)));
    f_map::(t1 -> t2) -> [t1] -> [t2];
    f_map a_f a_x=[a_f a_a|a_a<-a_x];
    f_map2::(t1 -> t2 -> t3) -> [t1] -> [t2] -> [t3];
    f_map2 a_f a_x a_y=[a_f a_a a_b|(a_a,a_b)<-f_zip2 a_x a_y];
    f_max::[Int] -> Int;
    f_max a_xs=f_foldl1 f_max2 a_xs;
    f_max2::Int -> Int -> Int;
    f_max2 a_a a_b=
        if (((>=) :: (Int -> Int -> Bool)) a_a a_b)
        then a_a
        else 
            a_b;
    f_member::[Int] -> Int -> Bool;
    f_member a_x a_a=f_or (f_map (flip ((==) :: (Int -> Int -> Bool)) a_a) a_x);
    f_merge::[Int] -> [Int] -> [Int];
    f_merge [] a_y=a_y;
    f_merge (a_a:a_x) []=(:) a_a a_x;
    f_merge (a_a:a_x) (a_b:a_y)=
        if (((<=) :: (Int -> Int -> Bool)) a_a a_b)
        then ((:) a_a (f_merge a_x ((:) a_b a_y)))
        else 
            ((:) a_b (f_merge ((:) a_a a_x) a_y));
    f_min::[Int] -> Int;
    f_min a_xs=f_foldl1 f_min2 a_xs;
    f_min2::Int -> Int -> Int;
    f_min2 a_a a_b=
        if (((>) :: (Int -> Int -> Bool)) a_a a_b)
        then a_b
        else 
            a_a;
    f_mkset::[Int] -> [Int];
    f_mkset []=[];
    f_mkset (a_a:a_x)=(:) a_a (f_filter (flip ((/=) :: (Int -> Int -> Bool)) a_a) (f_mkset a_x));
    f_or::[Bool] -> Bool;
    f_or a_xs=f_foldr (||) False a_xs;
    c_pi::Double;
    c_pi=((*) :: (Double -> Double -> Double)) (4.00000 :: Double) (((atan) :: (Double -> Double)) (1.00000 :: Double));
    f_postfix::t1 -> [t1] -> [t1];
    f_postfix a_a a_x=(++) a_x ((:) a_a []);
    f_product::[Int] -> Int;
    f_product a_xs=f_foldl ((*) :: (Int -> Int -> Int)) (1 :: Int) a_xs;
    f_rep::Int -> t1 -> [t1];
    f_rep a_n a_x=f_take a_n (f_repeat a_x);
    f_repeat::t1 -> [t1];
    f_repeat a_x=(:) a_x (f_repeat a_x);
    f_reverse::[t1] -> [t1];
    f_reverse a_xs=f_foldl (flip (:)) [] a_xs;
    f_rjustify::Int -> [Char] -> [Char];
    f_rjustify a_n a_s=(++) (f_spaces (((-) :: (Int -> Int -> Int)) a_n (length a_s))) a_s;
    f_scan::(t1 -> t2 -> t1) -> t1 -> [t2] -> [t1];
    f_scan a_op=
        let { 
            f_g a_r []=(:) a_r [];
            f_g a_r (a_a:a_x)=(:) a_r (f_g (a_op a_r a_a) a_x)
         } in  f_g;
    f_snd::(t1,t2) -> t2;
    f_snd (a_a,a_b)=a_b;
    f_sort::[Int] -> [Int];
    f_sort a_x=
        let { 
            r_n=length a_x;
            r_n2=((quot) :: (Int -> Int -> Int)) r_n (2 :: Int)
         } in  
            if (((<=) :: (Int -> Int -> Bool)) r_n (1 :: Int))
            then a_x
            else 
                (f_merge (f_sort (f_take r_n2 a_x)) (f_sort (f_drop r_n2 a_x)));
    f_spaces::Int -> [Char];
    f_spaces a_n=f_rep a_n ' ';
    f_subtract::Int -> Int -> Int;
    f_subtract a_x a_y=((-) :: (Int -> Int -> Int)) a_y a_x;
    f_sum::[Int] -> Int;
    f_sum a_xs=f_foldl ((+) :: (Int -> Int -> Int)) (0 :: Int) a_xs;
data 
    T_sys_message=F_Stdout [Char] | F_Stderr [Char] | F_Tofile [Char] [Char] | F_Closefile [Char] | F_Appendfile [Char] | F_System [Char] | F_Exit Int;
    f_take::Int -> [t1] -> [t1];
    f_take 0 a_x=[];
    f_take a_n (a_a:a_x)=(:) a_a (f_take (((-) :: (Int -> Int -> Int)) a_n (1 :: Int)) a_x);
    f_take a_n a_x=[];
    f_takewhile::(t1 -> Bool) -> [t1] -> [t1];
    f_takewhile a_f []=[];
    f_takewhile a_f (a_a:a_x)=
        if (a_f a_a)
        then ((:) a_a (f_takewhile a_f a_x))
        else 
            [];
    f_transpose::[[t1]] -> [[t1]];
    f_transpose a_x=
        let { 
            r_x'=f_takewhile pair a_x
         } in  
            if (null r_x')
            then []
            else 
                ((:) (f_map head r_x') (f_transpose (f_map tail r_x')));
    f_until::(t1 -> Bool) -> (t1 -> t1) -> t1 -> t1;
    f_until a_f a_g a_x=
        if (a_f a_x)
        then a_x
        else 
            (f_until a_f a_g (a_g a_x));
    f_zip2::[t1] -> [t2] -> [(t1,t2)];
    f_zip2 (a_a:a_x) (a_b:a_y)=(:) (a_a,a_b) (f_zip2 a_x a_y);
    f_zip2 a_x a_y=[];
    f_zip3 (a_a:a_x) (a_b:a_y) (a_c:a_z)=(:) (a_a,a_b,a_c) (f_zip3 a_x a_y a_z);
    f_zip3 a_x a_y a_z=[];
    f_zip4 (a_a:a_w) (a_b:a_x) (a_c:a_y) (a_d:a_z)=(:) (a_a,a_b,a_c,a_d) (f_zip4 a_w a_x a_y a_z);
    f_zip4 a_w a_x a_y a_z=[];
    f_zip5 (a_a:a_v) (a_b:a_w) (a_c:a_x) (a_d:a_y) (a_e:a_z)=(:) (a_a,a_b,a_c,a_d,a_e) (f_zip5 a_v a_w a_x a_y a_z);
    f_zip5 a_v a_w a_x a_y a_z=[];
    f_zip6 (a_a:a_u) (a_b:a_v) (a_c:a_w) (a_d:a_x) (a_e:a_y) (a_f:a_z)=(:) (a_a,a_b,a_c,a_d,a_e,a_f) (f_zip6 a_u a_v a_w a_x a_y a_z);
    f_zip6 a_u a_v a_w a_x a_y a_z=[];
    f_zip::([t1],[t2]) -> [(t1,t2)];
    f_zip (a_x,a_y)=f_zip2 a_x a_y;
    f_main a_x=f_benchmark_main a_x;
    main = do (n:_) <- getArgs; putStr (f_main (read n :: Int))
}
