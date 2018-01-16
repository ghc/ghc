module Main (main) -- listcompr
where {
--partain: import Fast2haskell;
#include "../Fast2haskell.hs"
    strict_show_i::Int -> [Char];
    strict_show_i x=miraseq x (show x);
    strict_show_d::Double -> [Char];
    strict_show_d x=miraseq x (show x);

type 
    T_var=[Char];
data 
    T_def=F_Def T_var [T_var] T_expr;
data 
    T_expr=F_Apl T_var [T_expr] | F_Cmp T_expr [T_qual] | F_Let [T_def] T_expr | F_Cns [Char] | F_Var T_var;
data 
    T_qual=F_Gen Int T_var T_expr | F_Fil T_expr;
    f_var_cmp::T_var -> T_var -> Int;
    f_var_cmp a_vx a_vy=
        let { 
            f_char_cmp a_x a_y=((-) :: (Int -> Int -> Int)) (fromEnum a_x) (fromEnum a_y)
         } in  f_list_cmp f_char_cmp a_vx a_vy;
    f_var_less,f_var_equal,f_var_greater::T_var -> T_var -> Bool;
    f_var_less a_vx a_vy=((<) :: (Int -> Int -> Bool)) (f_var_cmp a_vx a_vy) (0 :: Int);
    f_var_equal a_vx a_vy=((==) :: (Int -> Int -> Bool)) (f_var_cmp a_vx a_vy) (0 :: Int);
    f_var_greater a_vx a_vy=((>) :: (Int -> Int -> Bool)) (f_var_cmp a_vx a_vy) (0 :: Int);
    f_var_list_remove::[T_var] -> [T_var] -> [T_var];
    f_var_list_remove a_vxs a_vys=f_remove_cmp f_var_cmp a_vxs a_vys;
    f_def_cmp::T_def -> T_def -> Int;
    f_def_cmp (F_Def a_vx a_vxs a_ex) (F_Def a_vy a_vyx a_ey)=f_var_cmp a_vx a_vy;
    f_def_list_remove::[T_def] -> [T_def] -> [T_def];
    f_def_list_remove a_vxs a_vys=f_remove_cmp f_def_cmp a_vxs a_vys;
    c_prg::[T_def];
    c_prg=
        let { 
            r_va=F_Var "a";
            r_vb=F_Var "b";
            r_vc=F_Var "c";
            r_vd=F_Var "d";
            r_vh=F_Var "h";
            r_vq=F_Var "q";
            r_vx=F_Var "x";
            r_vy=F_Var "y";
            r_vz=F_Var "z";
            r_vxs=F_Var "xs";
            r_vys=F_Var "ys";
            r_zero=F_Cns "0";
            r_one=F_Cns "1";
            r_true=F_Cns "true";
            r_false=F_Cns "false"
         } in  (:) (F_Def "sqs" ((:) "xs" []) (F_Cmp (F_Apl "sq" ((:) r_vx [])) 
            ((:) (F_Gen (1 :: Int) "x" r_vxs) ((:) (F_Fil (F_Apl "odd" ((:) r_vx []))) [])))) ((:) 
            (F_Def "xys" ((:) "xs" ((:) "ys" [])) (F_Cmp (F_Apl "," ((:) r_vx ((:) r_vy []))) 
            ((:) (F_Gen (2 :: Int) "x" r_vxs) ((:) (F_Gen (3 :: Int) "y" r_vys) [])))) ((:) (F_Def "sq" ((:) "x" []) 
            (F_Apl "*" ((:) r_vx ((:) r_vx [])))) ((:) (F_Def "odd" ((:) "x" []) (F_Apl "if" 
            ((:) (F_Apl "=" ((:) (F_Apl "%" ((:) r_vx ((:) r_one []))) ((:) r_zero []))) 
            ((:) r_true ((:) r_false []))))) ((:) (F_Def "q" ((:) "c" ((:) "d" [])) (F_Let 
            ((:) (F_Def "e" ((:) "z" []) (F_Apl "*" ((:) r_vc ((:) r_vz [])))) ((:) 
            (F_Def "p" ((:) "a" ((:) "b" [])) (F_Let ((:) (F_Def "f" ((:) "x" 
            ((:) "y" [])) (F_Apl "*" ((:) r_vc ((:) r_vx [])))) ((:) (F_Def "g" ((:) "y" 
            ((:) "z" [])) (F_Apl "f" ((:) r_vb ((:) r_vy [])))) ((:) (F_Def "h" [] (F_Apl "g" 
            ((:) r_va ((:) r_vd [])))) []))) (F_Apl "f" ((:) (F_Apl "g" ((:) r_vh ((:) r_va []))) 
            ((:) r_vc []))))) ((:) (F_Def "q" [] (F_Apl "e" ((:) (F_Apl "p" ((:) r_vc 
            ((:) r_vd []))) []))) []))) (F_Apl "e" ((:) r_vq [])))) []))));
    f_sp::[T_def] -> [Char];
    f_sp a_ds=f_concat [(++) (f_sd a_d) ";\n"|a_d<-a_ds];
    f_sd::T_def -> [Char];
    f_sd (F_Def a_f a_bs a_e)=(++) a_f ((++) (f_concat [(++) " " a_b|a_b<-a_bs]) ((++) " = " (f_se a_e)));
    f_se::T_expr -> [Char];
    f_se (F_Apl a_h a_es)=
        let { 
            r_se'=
                if (not (f_letter (head a_h)))
                then ((++) "(" ((++) (f_se ((!!) a_es (0 :: Int))) ((++) a_h ((++) 
                    (f_se ((!!) a_es (1 :: Int))) ")"))))
                else 
                    ((++) "(" ((++) a_h ((++) (f_concat [(++) " " (f_se a_e)|a_e<-a_es]) ")")))
         } in  r_se';
    f_se (F_Cmp a_e a_qs)=(++) "[" ((++) (f_se a_e) ((++) " | " ((++) (tail 
        (f_concat [(++) ";" (f_sq a_q)|a_q<-a_qs])) "]")));
    f_se (F_Let [] a_e)=f_se a_e;
    f_se (F_Let a_ds a_e)=(++) "(LET " ((++) (tail (f_concat [(++) ";\n" (f_sd a_d)|a_d<-a_ds])) 
        ((++) " IN  " ((++) (f_se a_e) ")")));
    f_se (F_Cns a_c)=a_c;
    f_se (F_Var a_v)=a_v;
    f_indent::[Char] -> [Char];
    f_indent []=[];
    f_indent "\n"="\n";
    f_indent ('\o012':a_cs)=(++) "\n  " (f_indent a_cs);
    f_indent (a_c:a_cs)=(:) a_c (f_indent a_cs);
    f_sq::T_qual -> [Char];
    f_sq (F_Gen a_n a_v a_e)=(++) a_v ((++) " <- " (f_se a_e));
    f_sq (F_Fil a_e)=f_se a_e;
    f_tp::[T_def] -> [T_def];
    f_tp a_ds=f_map f_td a_ds;
    f_td::T_def -> T_def;
    f_td (F_Def a_f a_bs a_e)=F_Def a_f a_bs (f_te a_e a_f);
    f_te::T_expr -> T_var -> T_expr;
    f_te (F_Apl a_h a_es) a_f=F_Apl a_h [f_te a_e a_f|a_e<-a_es];
    f_te (F_Cmp a_e a_qs) a_f=f_tq (F_Cmp a_e a_qs) (F_Cns "[]") a_f;
    f_te (F_Let a_ds a_e) a_f=F_Let [F_Def a_d a_bs (f_te a_e a_f)|(F_Def a_d a_bs a_e)<-a_ds] (f_te a_e a_f);
    f_te (F_Cns a_c) a_f=F_Cns a_c;
    f_te (F_Var a_v) a_f=F_Var a_v;
    f_tq::T_expr -> T_expr -> T_var -> T_expr;
    f_tq (F_Cmp a_e ((F_Gen a_n a_v a_l1):a_qs)) a_l2 a_f=
        let { 
            r_h=(++) a_f ((++) "_h" (strict_show_i a_n));
            r_us=(++) a_f ((++) "_l" (strict_show_i a_n));
            r_us'=(++) a_f ((++) "_t" (strict_show_i a_n));
            r_vus=F_Var r_us;
            r_vus'=F_Var r_us'
         } in  F_Let ((:) (F_Def r_h ((:) r_us []) (F_Apl "if" ((:) 
            (F_Apl "null" ((:) r_vus [])) ((:) (f_te a_l2 a_f) ((:) (F_Let ((:) 
            (F_Def a_v [] (F_Apl "hd" ((:) r_vus []))) ((:) (F_Def r_us' [] (F_Apl "tl" ((:) r_vus []))) [])) 
            (f_tq (F_Cmp a_e a_qs) (F_Apl r_h ((:) r_vus' [])) a_f)) []))))) []) (F_Apl r_h ((:) (f_te a_l1 a_f) []));
    f_tq (F_Cmp a_e ((F_Fil a_b):a_qs)) a_l2 a_f=F_Apl "if" ((:) (f_te a_b a_f) ((:) (f_tq (F_Cmp a_e a_qs) a_l2 a_f) 
        ((:) (f_te a_l2 a_f) [])));
    f_tq (F_Cmp a_e []) a_l2 a_f=F_Apl ":" ((:) (f_te a_e a_f) ((:) (f_te a_l2 a_f) []));
    f_lp::[T_def] -> [T_def];
    f_lp a_p=f_concat [f_ld a_d []|a_d<-a_p];
    f_ld::T_def -> [T_var] -> [T_def];
    f_ld (F_Def a_f a_bs a_e) a_gs=
        let { 
            (r_p,r_e')=f_le a_e (f_usort ((++) a_bs a_gs))
         } in  (:) (F_Def a_f a_bs r_e') r_p;
    f_le::T_expr -> [T_var] -> ([T_def],T_expr);
    f_le (F_Apl a_h a_es) a_gs=
        let { 
            r_pes=[f_le a_e a_gs|a_e<-a_es]
         } in  (f_flatmap f_fst r_pes,F_Apl a_h (f_map f_snd r_pes));
    f_le (F_Cmp a_e a_qs) a_gs=([],F_Cmp a_e a_qs);
    f_le (F_Let a_ds a_e) a_gs=
        let { 
            (r_p,r_e')=f_le a_e a_gs;
            r_ds'=f_concat [f_ld a_d a_gs|a_d<-a_ds];
            r_cafs=[F_Def a_c [] a_e|(F_Def a_c [] a_e)<-r_ds'];
            r_funs=f_def_list_remove r_ds' r_cafs;
            r_names=[a_f|(F_Def a_f a_bs a_e)<-r_funs];
            r_frees=[f_ud a_d a_gs|a_d<-r_funs];
            r_calls=[f_ud a_d r_names|a_d<-r_funs];
            r_xtras=[(a_n,f_map F_Var a_xs)|(a_n,a_xs)<-f_zip2 r_names r_args];
            r_args=[f_usort ((++) a_fs (f_concat [a_fs'|(a_n',a_fs')<-f_zip2 r_names r_frees,a_c<-a_cs,f_var_equal a_c a_n']))|(a_fs,a_cs)<-f_zip2 r_frees r_calls];
            r_cafs'=[F_Def a_c [] (f_xe a_e r_xtras)|(F_Def a_c [] a_e)<-r_cafs];
            r_funs'=[F_Def a_f ((++) a_as a_bs) (f_xe a_e r_xtras)|(a_as,(F_Def a_f a_bs a_e))<-f_zip2 r_args r_funs]
         } in  ((++) r_p r_funs',F_Let r_cafs' (f_xe r_e' r_xtras));
    f_le (F_Cns a_c) a_gs=([],F_Cns a_c);
    f_le (F_Var a_v) a_gs=([],F_Var a_v);
    f_xe::T_expr -> [(T_var,[T_expr])] -> T_expr;
    f_xe (F_Apl a_h a_es) a_xs=
        let { 
            f_extend []=[];
            f_extend ((a_f,a_xs):a_xtra)=
                if (f_var_equal a_h a_f)
                then a_xs
                else 
                    (f_extend a_xtra)
         } in  F_Apl a_h ((++) (f_extend a_xs) [f_xe a_e a_xs|a_e<-a_es]);
    f_xe (F_Cmp a_e a_qs) a_xs=F_Cmp a_e a_qs;
    f_xe (F_Let a_ds a_e) a_xs=F_Let [F_Def a_d a_bs (f_xe a_e a_xs)|(F_Def a_d a_bs a_e)<-a_ds] (f_xe a_e a_xs);
    f_xe (F_Cns a_c) a_xs=F_Cns a_c;
    f_xe (F_Var a_v) a_xs=F_Var a_v;
    f_up::[T_def] -> [T_var] -> [T_var];
    f_up a_ds a_vs=f_concat [f_ud a_d a_vs|a_d<-a_ds];
    f_ud::T_def -> [T_var] -> [T_var];
    f_ud (F_Def a_f a_bs a_e) a_vs=f_ue a_e (f_var_list_remove a_vs a_bs);
    f_ue::T_expr -> [T_var] -> [T_var];
    f_ue (F_Apl a_h a_es) a_vs=f_concat [f_ue a_e a_vs|a_e<-a_es];
    f_ue (F_Cmp a_e a_qs) a_vs=[];
    f_ue (F_Let a_ds a_e) a_vs=(++) (f_up a_ds a_vs) (f_ue a_e a_vs);
    f_ue (F_Cns a_c) a_vs=[];
    f_ue (F_Var a_v') a_vs=[a_v|a_v<-a_vs,f_var_equal a_v a_v'];
    f_flatmap::(t1 -> [t2]) -> [t1] -> [t2];
    f_flatmap a_f []=[];
    f_flatmap a_f (a_x:a_xs)=(++) (a_f a_x) (f_flatmap a_f a_xs);
    f_usort::[T_var] -> [T_var];
    f_usort []=[];
    f_usort (a_x:a_xs)=(++) (f_usort [a_u|a_u<-a_xs,f_var_less a_u a_x]) ((:) a_x (f_usort [a_u|a_u<-a_xs,f_var_greater a_u a_x]));
    f_benchmark_main a_n=f_sp (f_tp (f_concat (f_take a_n (f_repeat c_prg))));
    c_cmp_less::Int;
    c_cmp_less=((negate) :: (Int -> Int)) (1 :: Int);
    c_cmp_equal::Int;
    c_cmp_equal=(0 :: Int);
    c_cmp_greater::Int;
    c_cmp_greater=(1 :: Int);
    f_cmp_c::Char -> Char -> Int;
    f_cmp_c a_x a_y=((-) :: (Int -> Int -> Int)) (fromEnum a_x) (fromEnum a_y);
    f_cmp_string::[Char] -> [Char] -> Int;
    f_cmp_string a_xs a_ys=f_list_cmp f_cmp_c a_xs a_ys;
    f_cmp_i::Int -> Int -> Int;
    f_cmp_i a_x a_y=((-) :: (Int -> Int -> Int)) a_x a_y;
    f_cmp_d::Double -> Double -> Int;
    f_cmp_d a_x a_y=floor (entier (((-) :: (Double -> Double -> Double)) a_x a_y));
    f_max_cmp::(t1 -> t1 -> Int) -> [t1] -> t1;
    f_max_cmp a_cmp a_xs=f_foldl1 (f_max2_cmp a_cmp) a_xs;
    f_max2_cmp::(t1 -> t1 -> Int) -> t1 -> t1 -> t1;
    f_max2_cmp a_cmp a_a a_b=
        if (((>=) :: (Int -> Int -> Bool)) (a_cmp a_a a_b) c_cmp_equal)
        then a_a
        else 
            a_b;
    f_member_cmp::(t1 -> t1 -> Int) -> [t1] -> t1 -> Bool;
    f_member_cmp a_cmp a_x a_a=f_or (f_map ((.) (flip ((==) :: (Int -> Int -> Bool)) c_cmp_equal) (a_cmp a_a)) a_x);
    f_min_cmp::(t1 -> t1 -> Int) -> [t1] -> t1;
    f_min_cmp a_cmp a_xs=f_foldl1 (f_min2_cmp a_cmp) a_xs;
    f_min2_cmp::(t1 -> t1 -> Int) -> t1 -> t1 -> t1;
    f_min2_cmp a_cmp a_a a_b=
        if (((>) :: (Int -> Int -> Bool)) (a_cmp a_a a_b) c_cmp_equal)
        then a_b
        else 
            a_a;
    f_mkset_cmp::(t1 -> t1 -> Int) -> [t1] -> [t1];
    f_mkset_cmp a_cmp []=[];
    f_mkset_cmp a_cmp (a_a:a_x)=(:) a_a (f_filter ((.) (flip ((/=) :: (Int -> Int -> Bool)) c_cmp_equal) (a_cmp a_a)) (f_mkset_cmp a_cmp a_x));
    f_uniqmerge_cmp::(t1 -> t1 -> Int) -> [t1] -> [t1] -> [t1];
    f_uniqmerge_cmp a_cmp [] a_y=a_y;
    f_uniqmerge_cmp a_cmp (a_a:a_x) []=(:) a_a a_x;
    f_uniqmerge_cmp a_cmp (a_a:a_x) (a_b:a_y)=
        let { 
            r_order=a_cmp a_a a_b
         } in  
            if (((<) :: (Int -> Int -> Bool)) r_order c_cmp_equal)
            then ((:) a_a (f_uniqmerge_cmp a_cmp a_x ((:) a_b a_y)))
            else 
            if (((==) :: (Int -> Int -> Bool)) r_order c_cmp_equal)
            then ((:) a_a (f_uniqmerge_cmp a_cmp a_x a_y))
            else 
                ((:) a_b (f_uniqmerge_cmp a_cmp ((:) a_a a_x) a_y));
    f_uniqsort_cmp::(t1 -> t1 -> Int) -> [t1] -> [t1];
    f_uniqsort_cmp a_cmp a_x=
        let { 
            r_n=length a_x;
            r_n2=((quot) :: (Int -> Int -> Int)) r_n (2 :: Int)
         } in  
            if (((<=) :: (Int -> Int -> Bool)) r_n (1 :: Int))
            then a_x
            else 
                (f_uniqmerge_cmp a_cmp (f_uniqsort_cmp a_cmp (f_take r_n2 a_x)) (f_uniqsort_cmp a_cmp (f_drop r_n2 a_x)));
    f_merge_cmp::(t1 -> t1 -> Int) -> [t1] -> [t1] -> [t1];
    f_merge_cmp a_cmp [] a_y=a_y;
    f_merge_cmp a_cmp (a_a:a_x) []=(:) a_a a_x;
    f_merge_cmp a_cmp (a_a:a_x) (a_b:a_y)=
        if (((<=) :: (Int -> Int -> Bool)) (a_cmp a_a a_b) c_cmp_equal)
        then ((:) a_a (f_merge_cmp a_cmp a_x ((:) a_b a_y)))
        else 
            ((:) a_b (f_merge_cmp a_cmp ((:) a_a a_x) a_y));
    f_sort_cmp::(t1 -> t1 -> Int) -> [t1] -> [t1];
    f_sort_cmp a_cmp a_x=
        let { 
            r_n=length a_x;
            r_n2=((quot) :: (Int -> Int -> Int)) r_n (2 :: Int)
         } in  
            if (((<=) :: (Int -> Int -> Bool)) r_n (1 :: Int))
            then a_x
            else 
                (f_merge_cmp a_cmp (f_sort_cmp a_cmp (f_take r_n2 a_x)) (f_sort_cmp a_cmp (f_drop r_n2 a_x)));
    f_list_cmp::(t1 -> t1 -> Int) -> [t1] -> [t1] -> Int;
    f_list_cmp a_cmp [] []=c_cmp_equal;
    f_list_cmp a_cmp [] a_ys=c_cmp_less;
    f_list_cmp a_cmp a_xs []=c_cmp_greater;
    f_list_cmp a_cmp (a_x:a_xs) (a_y:a_ys)=
        let { 
            r_order=a_cmp a_x a_y
         } in  
            if (((==) :: (Int -> Int -> Bool)) r_order c_cmp_equal)
            then (f_list_cmp a_cmp a_xs a_ys)
            else 
                r_order;
    f_remove_cmp::(t1 -> t1 -> Int) -> [t1] -> [t1] -> [t1];
    f_remove_cmp a_cmp a_xs []=a_xs;
    f_remove_cmp a_cmp a_xs (a_y:a_ys)=
        let { 
            f_remove' a_y []=[];
            f_remove' a_y (a_x:a_xs)=
                if (((==) :: (Int -> Int -> Bool)) (a_cmp a_x a_y) c_cmp_equal)
                then a_xs
                else 
                    ((:) a_x (f_remove' a_y a_xs))
         } in  f_remove_cmp a_cmp (f_remove' a_y a_xs) a_ys;
    f_cmp_combine::[Int] -> Int;
    f_cmp_combine []=c_cmp_equal;
    f_cmp_combine (a_x:a_xs)=
        if (((==) :: (Int -> Int -> Bool)) a_x c_cmp_equal)
        then (f_cmp_combine a_xs)
        else 
            a_x;
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
    c_input=(1000 :: Int);
    main = putStr (f_main c_input)
}
