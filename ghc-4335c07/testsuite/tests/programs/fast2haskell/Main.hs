module Main (main) -- TEST
where {
import Fast2haskell;
import Data.Complex;--1.3
import Data.Array;--1.3

    c_eps=(5.00000e-06 :: Double);
    c_t=True;
    c_f=False;
    c_input=(0 :: Int);
    f_main a_n=
        let { 
            r_x=[(a_tf,(++) (show a_i) ((++) "\t" a_str))|(a_i,(a_tf,a_str))<-f_zip2 (enumFrom (1 :: Int)) c_testlist];
            r_noks=[(++) a_str "\n"|(a_tf,a_str)<-r_x,not a_tf];
            r_oks=[(++) a_str "\n"|(a_tf,a_str)<-r_x,a_tf]
         } in  
            if (((>) :: (Int -> Int -> Bool)) a_n (0 :: Int))
            then (f_onetest ((!!) c_testlist (((-) :: (Int -> Int -> Int)) a_n (1 :: Int))))
            else 
                ((++) (show (length r_oks)) ((++) " tests passed and " ((++) (show 
                (length r_noks)) ((++) " failed\n" (c_concat r_noks)))));
    f_onetest (True,a_str)=(++) "true:  " ((++) a_str "\n");
    f_onetest (False,a_str)=(++) "false: " ((++) a_str "\n");
    f_booltest a_name True a_try=
        if a_try
        then (True,"")
        else 
            (False,(++) a_name "\tok: true is: false");
    f_booltest a_name False a_try=
        if (not a_try)
        then (True,"")
        else 
            (False,(++) a_name "\tok: false is: true");
    f_inttest a_name a_ok a_try=
        if (((==) :: (Int -> Int -> Bool)) a_ok a_try)
        then (True,"")
        else 
            (False,(++) a_name ((++) "\tok: " ((++) (show a_ok) ((++) "\tis: " 
            (show a_try)))));
    f_chartest a_name a_ok a_try=
        if (((==) :: (Int -> Int -> Bool)) (fromEnum a_ok) (fromEnum a_try))
        then (True,"")
        else 
            (False,(++) a_name ((++) "\tok: " ((++) ((:) a_ok []) ((++) "\tis: " 
            ((:) a_try [])))));
    f_strtest a_name a_ok a_try=
        if (strcmp a_ok a_try)
        then (True,"")
        else 
            (False,(++) a_name ((++) "\tok: " ((++) a_ok ((++) "\tis: " a_try))));
    f_linttest a_name a_ok a_try=
        if (f_lintcmp a_ok a_try)
        then (True,"")
        else 
            (False,(++) a_name ((++) "\tok: " ((++) (f_showlint a_ok) ((++) "\tis: " 
            (f_showlint a_try)))));
    f_doubtest a_name a_ok a_try=
        if (((<=) :: (Double -> Double -> Bool)) (f_abs (((-) :: (Double -> Double -> Double)) a_ok a_try)) c_eps)
        then (True,"")
        else 
            (False,(++) a_name ((++) "\tok: " ((++) (show a_ok) ((++) "\tis: " 
            ((++) (show a_try) ((++) "\tok-is: " (show (((-) :: (Double -> Double -> Double)) a_ok a_try))))))));
    f_alternating a_l=(:) (0 :: Int) ((:) (1 :: Int) a_l);
    f_showlint []=[];
    f_showlint a_xs=tail (c_concat [(++) "," (show a_x)|a_x<-a_xs]);
    f_lintcmp [] []=True;
    f_lintcmp [] a_ys=False;
    f_lintcmp a_xs []=False;
    f_lintcmp (a_x:a_xs) (a_y:a_ys)=
        if (((==) :: (Int -> Int -> Bool)) a_x a_y)
        then (f_lintcmp a_xs a_ys)
        else 
            False;
    c_testlist=(:) (f_inttest "array" (10 :: Int) ((!) (array (descr (1 :: Int) (3 :: Int)) ((:) 
        ((,) (3 :: Int) (30 :: Int)) ((:) ((,) (1 :: Int) (10 :: Int)) ((:) ((,) (2 :: Int) (20 :: Int)) [])))) (1 :: Int))) ((:) (f_inttest "array" (20 :: Int) 
        ((!) (array (descr (1 :: Int) (3 :: Int)) ((:) ((,) (3 :: Int) (30 :: Int)) ((:) ((,) (1 :: Int) (10 :: Int)) 
        ((:) ((,) (2 :: Int) (20 :: Int)) [])))) (2 :: Int))) ((:) (f_inttest "array" (30 :: Int) ((!) (array (descr (1 :: Int) (3 :: Int)) 
        ((:) ((,) (3 :: Int) (30 :: Int)) [])) (3 :: Int))) ((:) (f_inttest "assoc" (0 :: Int) (indassoc ((,) (0 :: Int) (1 :: Int)))) ((:) 
        (f_inttest "assoc" (1 :: Int) (valassoc ((,) (0 :: Int) (1 :: Int)))) ((:) (f_inttest "bounds" (1 :: Int) (lowbound (bounds 
        (listArray (descr (1 :: Int) (3 :: Int)) ((:) (1 :: Int) ((:) (2 :: Int) ((:) (3 :: Int) []))))))) ((:) (f_inttest "bounds" (3 :: Int) 
        (upbound (bounds (listArray (descr (1 :: Int) (3 :: Int)) ((:) (1 :: Int) ((:) (2 :: Int) ((:) (3 :: Int) []))))))) 
        ((:) (f_inttest "descr" (0 :: Int) (lowbound (descr (0 :: Int) (1 :: Int)))) ((:) (f_inttest "descr" (1 :: Int) (upbound 
        (descr (0 :: Int) (1 :: Int)))) ((:) (f_linttest "destr_update" ((:) (1 :: Int) ((:) (0 :: Int) ((:) (3 :: Int) []))) (elems 
        (destr_update (listArray (descr (0 :: Int) (2 :: Int)) ((:) (1 :: Int) ((:) (2 :: Int) ((:) (3 :: Int) [])))) (1 :: Int) (0 :: Int)))) ((:) 
        (f_linttest "destr_update" ((:) (0 :: Int) []) (elems (destr_update (listArray (descr (0 :: Int) (0 :: Int)) ((:) (1 :: Int) 
        ((:) (2 :: Int) ((:) (3 :: Int) [])))) (0 :: Int) (0 :: Int)))) ((:) (f_linttest "elems" ((:) (1 :: Int) ((:) (2 :: Int) ((:) (3 :: Int) []))) 
        (elems (listArray (descr (0 :: Int) (2 :: Int)) ((:) (1 :: Int) ((:) (2 :: Int) ((:) (3 :: Int) [])))))) ((:) 
        (f_linttest "elems" ((:) (1 :: Int) []) (elems (listArray (descr (0 :: Int) (0 :: Int)) ((:) (1 :: Int) ((:) (2 :: Int) 
        ((:) (3 :: Int) [])))))) ((:) (f_inttest "indassoc" (0 :: Int) (indassoc ((,) (0 :: Int) (1 :: Int)))) ((:) (f_linttest "listarray" 
        ((:) (1 :: Int) ((:) (2 :: Int) ((:) (3 :: Int) []))) (elems (listArray (descr (0 :: Int) (2 :: Int)) ((:) (1 :: Int) 
        ((:) (2 :: Int) ((:) (3 :: Int) [])))))) ((:) (f_linttest "listarray" ((:) (1 :: Int) []) (elems (listArray 
        (descr (0 :: Int) (0 :: Int)) ((:) (1 :: Int) ((:) (2 :: Int) ((:) (3 :: Int) [])))))) ((:) (f_inttest "lowbound" (0 :: Int) (lowbound 
        (descr (0 :: Int) (1 :: Int)))) ((:) (f_inttest "subscript" (1 :: Int) ((!) (tabulate ((!!) ((:) (1 :: Int) 
        ((:) (2 :: Int) ((:) (3 :: Int) [])))) (descr (0 :: Int) (2 :: Int))) (0 :: Int))) ((:) (f_inttest "subscript" (2 :: Int) ((!) (tabulate 
        ((!!) ((:) (1 :: Int) ((:) (2 :: Int) ((:) (3 :: Int) [])))) (descr (0 :: Int) (2 :: Int))) (1 :: Int))) ((:) (f_inttest "subscript" (3 :: Int) 
        ((!) (tabulate ((!!) ((:) (1 :: Int) ((:) (2 :: Int) ((:) (3 :: Int) [])))) (descr (0 :: Int) (2 :: Int))) (2 :: Int))) 
        ((:) (f_linttest "tabulate" ((:) (1 :: Int) ((:) (2 :: Int) ((:) (3 :: Int) []))) (elems (tabulate 
        ((!!) ((:) (1 :: Int) ((:) (2 :: Int) ((:) (3 :: Int) [])))) (descr (0 :: Int) (2 :: Int))))) ((:) (f_linttest "tabulate" 
        ((:) (1 :: Int) []) (elems (tabulate ((!!) ((:) (1 :: Int) ((:) (2 :: Int) ((:) (3 :: Int) [])))) 
        (descr (0 :: Int) (0 :: Int))))) ((:) (f_inttest "upbound" (1 :: Int) (upbound (descr (0 :: Int) (1 :: Int)))) ((:) (f_inttest "valassoc" (1 :: Int) 
        (valassoc ((,) (0 :: Int) (1 :: Int)))) ((:) (f_doubtest "add_x" (0.00000 :: Double) (realPart (((+) :: (Complex_type -> Complex_type -> Complex_type)) ((:+) (0.00000 :: Double) (0.00000 :: Double)) 
        ((:+) (0.00000 :: Double) (0.00000 :: Double))))) ((:) (f_doubtest "add_x" (0.00000 :: Double) (imagPart (((+) :: (Complex_type -> Complex_type -> Complex_type)) ((:+) (0.00000 :: Double) (0.00000 :: Double)) ((:+) (0.00000 :: Double) (0.00000 :: Double))))) 
        ((:) (f_doubtest "add_x" (4.00000 :: Double) (realPart (((+) :: (Complex_type -> Complex_type -> Complex_type)) ((:+) (1.00000 :: Double) (2.00000 :: Double)) ((:+) (3.00000 :: Double) (4.00000 :: Double))))) ((:) 
        (f_doubtest "add_x" (6.00000 :: Double) (imagPart (((+) :: (Complex_type -> Complex_type -> Complex_type)) ((:+) (1.00000 :: Double) (2.00000 :: Double)) ((:+) (3.00000 :: Double) (4.00000 :: Double))))) ((:) (f_doubtest "complex" (1.00000 :: Double) 
        (realPart ((:+) (1.00000 :: Double) (0.00000 :: Double)))) ((:) (f_doubtest "complex" (1.00000 :: Double) (imagPart ((:+) (0.00000 :: Double) (1.00000 :: Double)))) ((:) 
        (f_doubtest "complex_im" (0.00000 :: Double) (imagPart ((:+) (1.00000 :: Double) (0.00000 :: Double)))) ((:) (f_doubtest "complex_im" (1.00000 :: Double) (imagPart ((:+) (0.00000 :: Double) (1.00000 :: Double)))) 
        ((:) (f_doubtest "complex_re" (0.00000 :: Double) (realPart ((:+) (0.00000 :: Double) (1.00000 :: Double)))) ((:) (f_doubtest "complex_re" (1.00000 :: Double) (realPart 
        ((:+) (1.00000 :: Double) (0.00000 :: Double)))) ((:) (f_doubtest "mul_x" (0.00000 :: Double) (realPart (((*) :: (Complex_type -> Complex_type -> Complex_type)) ((:+) (0.00000 :: Double) (0.00000 :: Double)) ((:+) (0.00000 :: Double) (0.00000 :: Double))))) 
        ((:) (f_doubtest "mul_x" (0.00000 :: Double) (imagPart (((*) :: (Complex_type -> Complex_type -> Complex_type)) ((:+) (0.00000 :: Double) (0.00000 :: Double)) ((:+) (0.00000 :: Double) (0.00000 :: Double))))) ((:) 
        (f_doubtest "mul_x" (((negate) :: (Double -> Double)) (5.00000 :: Double)) (realPart (((*) :: (Complex_type -> Complex_type -> Complex_type)) ((:+) (1.00000 :: Double) (2.00000 :: Double)) ((:+) (3.00000 :: Double) (4.00000 :: Double))))) ((:) 
        (f_doubtest "mul_x" (10.0000 :: Double) (imagPart (((*) :: (Complex_type -> Complex_type -> Complex_type)) ((:+) (1.00000 :: Double) (2.00000 :: Double)) ((:+) (3.00000 :: Double) (4.00000 :: Double))))) ((:) (f_doubtest "sub_x" (0.00000 :: Double) 
        (realPart (((-) :: (Complex_type -> Complex_type -> Complex_type)) ((:+) (0.00000 :: Double) (0.00000 :: Double)) ((:+) (0.00000 :: Double) (0.00000 :: Double))))) ((:) (f_doubtest "sub_x" (0.00000 :: Double) (imagPart 
        (((-) :: (Complex_type -> Complex_type -> Complex_type)) ((:+) (0.00000 :: Double) (0.00000 :: Double)) ((:+) (0.00000 :: Double) (0.00000 :: Double))))) ((:) (f_doubtest "sub_x" (((negate) :: (Double -> Double)) (2.00000 :: Double)) (realPart 
        (((-) :: (Complex_type -> Complex_type -> Complex_type)) ((:+) (1.00000 :: Double) (2.00000 :: Double)) ((:+) (3.00000 :: Double) (4.00000 :: Double))))) ((:) (f_doubtest "sub_x" (((negate) :: (Double -> Double)) (2.00000 :: Double)) (imagPart 
        (((-) :: (Complex_type -> Complex_type -> Complex_type)) ((:+) (1.00000 :: Double) (2.00000 :: Double)) ((:+) (3.00000 :: Double) (4.00000 :: Double))))) ((:) (f_inttest "seq" (2 :: Int) (seq (enumFrom (1 :: Int)) (2 :: Int))) 
        ((:) (f_strtest "**" "this one" "should fail") [])))))))))))))))))))))))))))))))))))))))))));
    f_abs a_x=
        if (((<=) :: (Double -> Double -> Bool)) a_x (0.00000 :: Double))
        then (((negate) :: (Double -> Double)) a_x)
        else 
            a_x;
    c_and=f_foldr (&&) True;
    f_cjustify a_n a_s=
        let { 
            r_margin=((-) :: (Int -> Int -> Int)) a_n (length a_s);
            r_lmargin=((div) :: (Int -> Int -> Int)) r_margin (2 :: Int);
            r_rmargin=((-) :: (Int -> Int -> Int)) r_margin r_lmargin
         } in  (++) (f_spaces r_lmargin) ((++) a_s (f_spaces r_rmargin));
    c_concat=f_foldr (++) [];
    f_const a_x a_y=a_x;
    f_digit a_x=
        if (((<=) :: (Int -> Int -> Bool)) (fromEnum '0') (fromEnum a_x))
        then (((<=) :: (Int -> Int -> Bool)) (fromEnum a_x) (fromEnum '9'))
        else 
            False;
    f_drop 0 a_x=a_x;
    f_drop a_n (a_a:a_x)=f_drop (((-) :: (Int -> Int -> Int)) a_n (1 :: Int)) a_x;
    f_drop a_n a_x=[];
    f_dropwhile a_f []=[];
    f_dropwhile a_f (a_a:a_x)=
        if (a_f a_a)
        then (f_dropwhile a_f a_x)
        else 
            ((:) a_a a_x);
    c_e=((exp) :: (Double -> Double)) (1.00000 :: Double);
    f_filter a_f a_x=[a_a|a_a<-a_x,a_f a_a];
    f_foldl a_op a_r []=a_r;
    f_foldl a_op a_r (a_a:a_x)=
        let { 
            f_strict a_f a_x=seq a_x (a_f a_x)
         } in  f_foldl a_op (f_strict a_op a_r a_a) a_x;
    f_foldl1 a_op (a_a:a_x)=f_foldl a_op a_a a_x;
    f_foldr a_op a_r []=a_r;
    f_foldr a_op a_r (a_a:a_x)=a_op a_a (f_foldr a_op a_r a_x);
    f_foldr1 a_op (a_a:[])=a_a;
    f_foldr1 a_op (a_a:a_b:a_x)=a_op a_a (f_foldr1 a_op ((:) a_b a_x));
    f_fst (a_a,a_b)=a_a;
    f_id a_x=a_x;
    f_index a_x=
        let { 
            f_f a_n []=[];
            f_f a_n (a_a:a_x)=(:) a_n (f_f (((+) :: (Int -> Int -> Int)) a_n (1 :: Int)) a_x)
         } in  f_f (0 :: Int) a_x;
    f_init (a_a:a_x)=
        if (null a_x)
        then []
        else 
            ((:) a_a (f_init a_x));
    f_iterate a_f a_x=(:) a_x (f_iterate a_f (a_f a_x));
    f_last a_x=(!!) a_x (((-) :: (Int -> Int -> Int)) (length a_x) (1 :: Int));
    f_lay []=[];
    f_lay (a_a:a_x)=(++) a_a ((++) "\n" (f_lay a_x));
    f_layn a_x=
        let { 
            f_f a_n []=[];
            f_f a_n (a_a:a_x)=(++) (f_rjustify (4 :: Int) (show a_n)) ((++) ") " ((++) a_a ((++) "\n" 
                (f_f (((+) :: (Int -> Int -> Int)) a_n (1 :: Int)) a_x))))
         } in  f_f (1 :: Int) a_x;
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
    f_limit (a_a:a_b:a_x)=
        if (((==) :: (Double -> Double -> Bool)) a_a a_b)
        then a_a
        else 
            (f_limit ((:) a_b a_x));
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
    f_ljustify a_n a_s=(++) a_s (f_spaces (((-) :: (Int -> Int -> Int)) a_n (length a_s)));
    f_map a_f a_x=[a_f a_a|a_a<-a_x];
    f_map2 a_f a_x a_y=[a_f a_a a_b|(a_a,a_b)<-f_zip2 a_x a_y];
    f_max a_xs=f_foldl1 f_max2 a_xs;
    f_max2 a_a a_b=
        if (((>=) :: (Int -> Int -> Bool)) a_a a_b)
        then a_a
        else 
            a_b;
    f_member a_x a_a=c_or (f_map (flip ((==) :: (Int -> Int -> Bool)) a_a) a_x);
    f_merge [] a_y=a_y;
    f_merge (a_a:a_x) []=(:) a_a a_x;
    f_merge (a_a:a_x) (a_b:a_y)=
        if (((<=) :: (Int -> Int -> Bool)) a_a a_b)
        then ((:) a_a (f_merge a_x ((:) a_b a_y)))
        else 
            ((:) a_b (f_merge ((:) a_a a_x) a_y));
    f_min a_xs=f_foldl1 f_min2 a_xs;
    f_min2 a_a a_b=
        if (((>) :: (Int -> Int -> Bool)) a_a a_b)
        then a_b
        else 
            a_a;
    f_mkset []=[];
    f_mkset (a_a:a_x)=(:) a_a (f_filter (flip ((/=) :: (Int -> Int -> Bool)) a_a) (f_mkset a_x));
    c_or=f_foldr (||) False;
    c_pi=((*) :: (Double -> Double -> Double)) (4.00000 :: Double) (((atan) :: (Double -> Double)) (1.00000 :: Double));
    f_postfix a_a a_x=(++) a_x ((:) a_a []);
    c_product=f_foldl ((*) :: (Int -> Int -> Int)) (1 :: Int);
    f_rep a_n a_x=f_take a_n (f_repeat a_x);
    f_repeat a_x=(:) a_x (f_repeat a_x);
    c_reverse=f_foldl (flip (:)) [];
    f_rjustify a_n a_s=(++) (f_spaces (((-) :: (Int -> Int -> Int)) a_n (length a_s))) a_s;
    f_scan a_op=
        let { 
            f_g a_r []=(:) a_r [];
            f_g a_r (a_a:a_x)=(:) a_r (f_g (a_op a_r a_a) a_x)
         } in  f_g;
    f_snd (a_a,a_b)=a_b;
    f_sort a_x=
        let { 
            r_n=length a_x;
            r_n2=((div) :: (Int -> Int -> Int)) r_n (2 :: Int)
         } in  
            if (((<=) :: (Int -> Int -> Bool)) r_n (1 :: Int))
            then a_x
            else 
                (f_merge (f_sort (f_take r_n2 a_x)) (f_sort (f_drop r_n2 a_x)));
    f_spaces a_n=f_rep a_n ' ';
    f_subtract a_x a_y=((-) :: (Int -> Int -> Int)) a_y a_x;
    c_sum=f_foldl ((+) :: (Int -> Int -> Int)) (0 :: Int);
data 
    T_sys_message=F_Stdout [Char] | F_Stderr [Char] | F_Tofile [Char] [Char] | F_Closefile [Char] | F_Appendfile [Char] | F_System [Char] | F_Exit Int;
    f_take 0 a_x=[];
    f_take a_n (a_a:a_x)=(:) a_a (f_take (((-) :: (Int -> Int -> Int)) a_n (1 :: Int)) a_x);
    f_take a_n a_x=[];
    f_takewhile a_f []=[];
    f_takewhile a_f (a_a:a_x)=
        if (a_f a_a)
        then ((:) a_a (f_takewhile a_f a_x))
        else 
            [];
    f_transpose a_x=
        let { 
            r_x'=f_takewhile pair a_x
         } in  
            if (null r_x')
            then []
            else 
                ((:) (f_map head r_x') (f_transpose (f_map tail r_x')));
    f_until a_f a_g a_x=
        if (a_f a_x)
        then a_x
        else 
            (f_until a_f a_g (a_g a_x));
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
    f_zip (a_x,a_y)=f_zip2 a_x a_y;
    main = putStr (f_main c_input)
}
