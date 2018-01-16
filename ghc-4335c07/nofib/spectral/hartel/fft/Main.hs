module Main (main) -- fft
where {
    import System.Environment (getArgs);
--partain: import Fast2haskell;
#include "../Fast2haskell.hs"
    strict_show_i::Int -> [Char];
    strict_show_i x=miraseq x (show x);
    strict_show_d::Double -> [Char];
    strict_show_d x=miraseq x (show x);

    f_my_cmp a_x a_y=
        if (((==) :: (Int -> Int -> Bool)) (fromEnum (f_cmp a_x a_y)) (fromEnum 't'))
        then "t"
        else 
            ((++) "f(" ((++) (f_showcomplex a_x) ((++) "-" ((++) (f_showcomplex a_y) 
            ((++) "=" ((++) (f_showcomplex (((-) :: (Complex_type -> Complex_type -> Complex_type)) a_x a_y)) ")\n"))))));
    f_benchmark_main a_n=
        let { 
            r_x=f_large a_n (64 :: Int)
         } in  (++) (f_sumcode (f_concat (f_map2 f_my_cmp (f_iaamain r_x) (f_rllmain r_x)))) "\n";
    f_sumcode::[Char] -> [Char];
    f_sumcode a_xs=
        let { 
            f_sumcode' [] a_sum a_n=(++) (strict_show_i (((+) :: (Int -> Int -> Int)) a_sum a_n)) ((:) '/' (strict_show_i a_n));
            f_sumcode' (a_x:a_xs) a_sum a_n=f_sumcode' a_xs (((+) :: (Int -> Int -> Int)) a_sum (fromEnum a_x)) (((+) :: (Int -> Int -> Int)) a_n (1 :: Int))
         } in  f_sumcode' a_xs (0 :: Int) (0 :: Int);
type 
    T_complex_array=Array_type Complex_type;
    f_iaafft::Int -> Int -> T_complex_array -> T_complex_array;
    f_iaafft a_size 0 a_xs=a_xs;
    f_iaafft a_size a_n a_xs=
        let { 
            r_m=f_log2 (((quot) :: (Int -> Int -> Int)) a_size (((*) :: (Int -> Int -> Int)) a_n (2 :: Int)));
            r_xs'=array (bounds a_xs) (f_concat [f_mkpair a_j|a_j<-[(0 :: Int)..((-) :: (Int -> Int -> Int)) a_size (1 :: Int)],
                ((==) :: (Int -> Int -> Bool)) (land_i a_j (lshift_i (1 :: Int) r_m)) (0 :: Int)]);
            f_mkpair a_j=
                let { 
                    r_x=(!) a_xs a_j;
                    r_y=(!) a_xs r_k;
                    r_z=((*) :: (Complex_type -> Complex_type -> Complex_type)) (f_unitroot a_size (((*) :: (Int -> Int -> Int)) a_n a_j)) r_y;
                    r_k=((+) :: (Int -> Int -> Int)) a_j (f_pow2 r_m)
                 } in  (:) ((,) a_j (((+) :: (Complex_type -> Complex_type -> Complex_type)) r_x r_z)) ((:) ((,) r_k (((-) :: (Complex_type -> Complex_type -> Complex_type)) r_x r_z)) [])
         } in  f_iaafft a_size (((quot) :: (Int -> Int -> Int)) a_n (2 :: Int)) r_xs';
    f_rllfft::Int -> Int -> [Complex_type] -> [Complex_type];
    f_rllfft a_size a_n (a_x:[])=(:) a_x [];
    f_rllfft a_size a_n a_xs=
        let { 
            r_ls'=f_map2 ((+) :: (Complex_type -> Complex_type -> Complex_type)) r_ls r_rs'';
            r_rs'=f_map2 ((-) :: (Complex_type -> Complex_type -> Complex_type)) r_ls r_rs'';
            r_rs''=f_map (((*) :: (Complex_type -> Complex_type -> Complex_type)) (f_unitroot a_size a_n)) r_rs;
            (r_ls,r_rs)=f_split (((quot) :: (Int -> Int -> Int)) (length a_xs) (2 :: Int)) a_xs
         } in  (++) (f_rllfft a_size (((quot) :: (Int -> Int -> Int)) a_n (2 :: Int)) r_ls') (f_rllfft a_size (((+) :: (Int -> Int -> Int)) (((quot) :: (Int -> Int -> Int)) a_n (2 :: Int)) 
            (((quot) :: (Int -> Int -> Int)) a_size (4 :: Int))) r_rs');
    f_bfly::Int -> Int -> Complex_type -> Complex_type -> (Complex_type,Complex_type);
    f_bfly a_i a_n a_x a_y=
        let { 
            r_z=((*) :: (Complex_type -> Complex_type -> Complex_type)) (f_unitroot a_i a_n) a_y
         } in  (((+) :: (Complex_type -> Complex_type -> Complex_type)) a_x r_z,((-) :: (Complex_type -> Complex_type -> Complex_type)) a_x r_z);
    f_unitroot::Int -> Int -> Complex_type;
    f_unitroot a_i a_n=
        let { 
            r_phi=((*) :: (Double -> Double -> Double)) (((/) :: (Double -> Double -> Double)) (fromIntegral (((*) :: (Int -> Int -> Int)) (2 :: Int) a_n)) (fromIntegral a_i)) c_pi
         } in  (:+) (((cos) :: (Double -> Double)) r_phi) (((sin) :: (Double -> Double)) r_phi);
    f_pow2::Int -> Int;
    f_pow2 a_x=lshift_i (1 :: Int) a_x;
    f_log2::Int -> Int;
    f_log2 a_x=floor (f_round_d (((/) :: (Double -> Double -> Double)) (((log) :: (Double -> Double)) (fromIntegral a_x)) (((log) :: (Double -> Double)) (2.00000 :: Double))));
    f_round_d::Double -> Double;
    f_round_d a_x=entier (((+) :: (Double -> Double -> Double)) a_x (0.500000 :: Double));
    f_split::Int -> [t1] -> ([t1],[t1]);
    f_split a_n a_xs=(f_take a_n a_xs,f_drop a_n a_xs);
    f_join::Int -> [t1] -> [t1] -> [t1];
    f_join a_n [] []=[];
    f_join a_n a_x a_y=
        let { 
            (r_firstx,r_restx)=f_split a_n a_x;
            (r_firsty,r_resty)=f_split a_n a_y
         } in  (++) r_firstx ((++) r_firsty (f_join a_n r_restx r_resty));
    f_reorder::Int -> [t1] -> [t1];
    f_reorder 1 a_y=a_y;
    f_reorder a_n a_y=
        let { 
            (r_left,r_right)=f_split (((quot) :: (Int -> Int -> Int)) r_size (2 :: Int)) a_y;
            r_m=((quot) :: (Int -> Int -> Int)) r_size a_n;
            r_size=length a_y
         } in  f_reorder (((quot) :: (Int -> Int -> Int)) a_n (2 :: Int)) (f_join r_m r_left r_right);
    f_rev_bits::Int -> Int -> Int;
    f_rev_bits a_wid a_x=
        let { 
            f_rev_bits' a_w a_x a_a=
                if (((==) :: (Int -> Int -> Bool)) a_w (0 :: Int))
                then a_a
                else 
                    (f_rev_bits' (((-) :: (Int -> Int -> Int)) a_w (1 :: Int)) (rshift_i a_x (1 :: Int)) (lor_i (lshift_i a_a (1 :: Int)) (land_i a_x (1 :: Int))))
         } in  f_rev_bits' a_wid a_x (0 :: Int);
    f_reorderindex::Int -> Array_type Int;
    f_reorderindex a_size=tabulate (f_rev_bits (f_log2 a_size)) (descr (0 :: Int) (((-) :: (Int -> Int -> Int)) a_size (1 :: Int)));
    f_aareorder::(Array_type Int) -> (Array_type t1) -> Array_type t1;
    f_aareorder a_index a_ar=
        let { 
            f_aareorder' a_i=(!) a_ar ((!) a_index a_i)
         } in  tabulate f_aareorder' (bounds a_ar);
    f_intplex::Int -> Int -> Complex_type;
    f_intplex a_r a_i=(:+) (fromIntegral a_r) (fromIntegral a_i);
    c_input1::[Complex_type];
    c_input1=
        let { 
            r_as=(:) (0 :: Int) ((:) (1 :: Int) ((:) (2 :: Int) ((:) (3 :: Int) ((:) (4 :: Int) ((:) (5 :: Int) 
                ((:) (6 :: Int) ((:) (7 :: Int) r_as)))))));
            r_bs=(:) (0 :: Int) ((:) (1 :: Int) ((:) (2 :: Int) ((:) (1 :: Int) ((:) (0 :: Int) r_bs))))
         } in  f_take (16 :: Int) (f_map2 f_intplex r_as r_bs);
    c_input2=(:) (f_intplex (2 :: Int) (3 :: Int)) ((:) (f_intplex (6 :: Int) (7 :: Int)) ((:) (f_intplex (4 :: Int) (5 :: Int)) 
        ((:) (f_intplex (8 :: Int) (9 :: Int)) [])));
    c_input3=f_map2 f_intplex ((:) (1 :: Int) ((:) (1 :: Int) ((:) (1 :: Int) ((:) (1 :: Int) ((:) (1 :: Int) 
        ((:) (1 :: Int) ((:) (1 :: Int) ((:) (1 :: Int) ((:) (1 :: Int) ((:) (((negate) :: (Int -> Int)) (1 :: Int)) ((:) 
        (((negate) :: (Int -> Int)) (1 :: Int)) ((:) (((negate) :: (Int -> Int)) (1 :: Int)) ((:) (((negate) :: (Int -> Int)) (1 :: Int)) ((:) (((negate) :: (Int -> Int)) (1 :: Int)) 
        ((:) (((negate) :: (Int -> Int)) (1 :: Int)) ((:) (((negate) :: (Int -> Int)) (1 :: Int)) [])))))))))))))))) [(0 :: Int),(0 :: Int)..];
    c_input4=f_large (5 :: Int) (64 :: Int);
    f_extend::Int -> [t1] -> [t1];
    f_extend 0 a_a=a_a;
    f_extend a_n a_a=f_extend (((-) :: (Int -> Int -> Int)) a_n (1 :: Int)) ((++) a_a a_a);
    f_large::Int -> Int -> [Complex_type];
    f_large a_coarse a_fine=f_extend a_coarse [f_intplex (((-) :: (Int -> Int -> Int)) a_fine a_i) (0 :: Int)|a_i<-[(1 :: Int)..a_fine]];
    f_cmp::Complex_type -> Complex_type -> Char;
    f_cmp a_ab a_cd=
        let { 
            r_a=realPart a_ab;
            r_b=imagPart a_ab;
            r_c=realPart a_cd;
            r_d=imagPart a_cd
         } in  
            if (
                if (((<=) :: (Double -> Double -> Bool)) (f_abs (((-) :: (Double -> Double -> Double)) r_a r_c)) c_eps)
                then (((<=) :: (Double -> Double -> Bool)) (f_abs (((-) :: (Double -> Double -> Double)) r_b r_d)) c_eps)
                else 
                    False)
            then 't'
            else 
                'f';
    f_showcomplexarray::T_complex_array -> [Char];
    f_showcomplexarray a_ar=
        let { 
            r_lu=bounds a_ar;
            r_l=lowbound r_lu;
            r_u=upbound r_lu
         } in  (++) "[" ((++) (f_showcomplex ((!) a_ar r_l)) ((++) (f_concat [
            (++) ", " (f_showcomplex ((!) a_ar a_i))|a_i<-[((+) :: (Int -> Int -> Int)) r_l (1 :: Int)..r_u]]) "] "));
    f_showcomplexlist::[Complex_type] -> [Char];
    f_showcomplexlist (a_a:a_as)=(++) "[" ((++) (f_showcomplex a_a) ((++) (f_concat [(++) ", " 
        (f_showcomplex a_a')|a_a'<-a_as]) "] "));
    f_showcomplex::Complex_type -> [Char];
    f_showcomplex a_ri=(++) "C " ((++) (f_showreal (realPart a_ri)) ((++) " " (f_showreal 
        (imagPart a_ri))));
    f_showreal::Double -> [Char];
    f_showreal a_r=
        if (((<=) :: (Double -> Double -> Bool)) (f_abs a_r) c_eps)
        then "0"
        else 
            (strict_show_d a_r);
    c_eps=(0.500000 :: Double);
    f_iaamain::[Complex_type] -> [Complex_type];
    f_iaamain a_xs=
        let { 
            r_index=f_reorderindex r_size;
            r_size=length a_xs
         } in  elems (f_iaafft r_size (((quot) :: (Int -> Int -> Int)) r_size (2 :: Int)) (f_aareorder r_index (listArray (descr (0 :: Int) 
            (((-) :: (Int -> Int -> Int)) r_size (1 :: Int))) a_xs)));
    f_iaashow::[Complex_type] -> [Char];
    f_iaashow a_xs=f_showcomplexlist (f_iaamain a_xs);
    f_rllmain::[Complex_type] -> [Complex_type];
    f_rllmain a_xs=
        let { 
            r_size=length a_xs
         } in  f_reorder r_size (f_rllfft r_size (0 :: Int) a_xs);
    f_rllshow::[Complex_type] -> [Char];
    f_rllshow a_xs=f_showcomplexlist (f_rllmain a_xs);
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
