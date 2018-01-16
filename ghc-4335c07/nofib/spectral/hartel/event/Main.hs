module Main (main) -- event
where {
    import System.Environment (getArgs);
--partain: import Fast2haskell;
#include "../Fast2haskell.hs"
    strict_show_i::Int -> [Char];
    strict_show_i x=miraseq x (show x);
    strict_show_d::Double -> [Char];
    strict_show_d x=miraseq x (show x);

data 
    T_threestate=C_X | C_L | C_H;
    f_showstate::T_threestate -> [Char];
    f_showstate C_H="H";
    f_showstate C_L="L";
    f_showstate a_x="x";
    f_nandfun::T_threestate -> T_threestate -> T_threestate;
    f_nandfun C_H C_H=C_L;
    f_nandfun C_L a_x=C_H;
    f_nandfun a_x C_L=C_H;
    f_nandfun a_x a_y=C_X;
    f_threestate_cmp::T_threestate -> T_threestate -> Int;
    f_threestate_cmp C_H C_H=(0 :: Int);
    f_threestate_cmp C_L C_L=(0 :: Int);
    f_threestate_cmp C_X C_X=(0 :: Int);
    f_threestate_cmp a_x a_y=(1 :: Int);
type 
    T_comb2=Int -> Int -> Int -> Int -> T_state -> T_event;
data 
    T_func2=C_NoFn | F_Fn T_comb2 Int Int;
data 
    T_dep=F_Pak Int T_func2;
data 
    T_event=F_At Int Int T_threestate;
type 
    T_state=[T_threestate];
    f_event_cmp::T_event -> T_event -> Int;
    f_event_cmp (F_At a_atime a_awire a_awhat) (F_At a_btime a_bwire a_bwhat)=
        if (((<) :: (Int -> Int -> Bool)) a_atime a_btime)
        then c_cmp_less
        else 
        if (((>) :: (Int -> Int -> Bool)) a_atime a_btime)
        then c_cmp_greater
        else 
            c_cmp_equal;
    f_update1::T_state -> Int -> T_threestate -> T_state;
    f_update1 a_st a_i a_val=(++) (f_take a_i a_st) ((:) a_val (f_drop (((+) :: (Int -> Int -> Int)) a_i (1 :: Int)) a_st));
    f_simulate::[T_event] -> T_state -> Int -> [T_event];
    f_simulate [] a_st a_et=[];
    f_simulate ((F_At a_time a_wire a_what):a_es) a_st a_et=
        let { 
            r_e=F_At a_time a_wire a_what;
            r_newes=f_merge_cmp f_event_cmp a_es (f_sort_cmp f_event_cmp r_more);
            r_newst=f_update1 a_st a_wire a_what;
            r_more=[f_mkevent a_out|a_out<-f_dependencies a_wire];
            f_mkevent a_wire=f_recalculate (f_d a_wire) a_time a_wire r_newst
         } in  
            if (((>=) :: (Int -> Int -> Bool)) a_time a_et)
            then []
            else 
            if (((==) :: (Int -> Int -> Bool)) (f_threestate_cmp ((!!) a_st a_wire) a_what) (0 :: Int))
            then (f_simulate a_es a_st a_et)
            else 
                ((:) r_e (f_simulate r_newes r_newst a_et));
    f_nand::T_comb2;
    f_nand a_x a_y a_time a_wire a_st=F_At (((+) :: (Int -> Int -> Int)) a_time (3 :: Int)) a_wire (f_nandfun ((!!) a_st a_x) ((!!) a_st a_y));
    f_clock::Int -> Int -> Int -> Int -> [T_event];
    f_clock a_wire a_t a_low a_high=(:) (F_At a_t a_wire C_L) ((:) (F_At (((+) :: (Int -> Int -> Int)) a_t a_low) a_wire C_H) (f_clock a_wire 
        (((+) :: (Int -> Int -> Int)) (((+) :: (Int -> Int -> Int)) a_t a_low) a_high) a_low a_high));
    f_findlaststate::[T_event] -> [T_threestate] -> [T_threestate];
    f_findlaststate [] a_st=a_st;
    f_findlaststate ((F_At a_t a_wire a_what):a_es) a_st=f_findlaststate a_es (f_update1 a_st a_wire a_what);
    f_recalculate::T_func2 -> Int -> Int -> [T_threestate] -> T_event;
    f_recalculate (F_Fn a_f a_x a_y)=a_f a_x a_y;
    c_getdep::[[Int]];
    c_getdep=f_foldl f_inserte [[]|a_i<-[(1 :: Int)..c_nwires]] [F_Pak a_x (f_d a_x)|a_x<-[(0 :: Int)..((-) :: (Int -> Int -> Int)) c_nwires (1 :: Int)]];
    f_update2::[[Int]] -> Int -> [Int] -> [[Int]];
    f_update2 a_st a_i a_val=(++) (f_take a_i a_st) ((:) a_val (f_drop (((+) :: (Int -> Int -> Int)) a_i (1 :: Int)) a_st));
    f_inserte::[[Int]] -> T_dep -> [[Int]];
    f_inserte a_x (F_Pak a_y C_NoFn)=a_x;
    f_inserte a_x (F_Pak a_y (F_Fn a_a a_b a_c))=f_update2 (f_update2 a_x a_b (f_unify ((!!) a_x a_b) a_y)) a_c (f_unify ((!!) a_x a_c) a_y);
    f_unify::[Int] -> Int -> [Int];
    f_unify [] a_n=(:) a_n [];
    f_unify (a_a:a_b) a_n=
        if (((==) :: (Int -> Int -> Bool)) a_a a_n)
        then ((:) a_a a_b)
        else 
            ((:) a_a (f_unify a_b a_n));
    f_dependencies::Int -> [Int];
    f_dependencies a_i=(!!) c_getdep a_i;
    c_istate::[T_threestate];
    c_istate=[C_X|a_i<-[(1 :: Int)..c_nwires]];
    c_nwires=(13 :: Int);
    c_ievents::[T_event];
    c_ievents=f_merge_cmp f_event_cmp (f_clock (1 :: Int) (0 :: Int) (50 :: Int) (50 :: Int)) (f_clock (0 :: Int) (25 :: Int) (100 :: Int) (100 :: Int));
    f_d::Int -> T_func2;
    f_d 3=F_Fn f_nand (0 :: Int) (0 :: Int);
    f_d 4=F_Fn f_nand (0 :: Int) (1 :: Int);
    f_d 5=F_Fn f_nand (3 :: Int) (1 :: Int);
    f_d 6=F_Fn f_nand (4 :: Int) (7 :: Int);
    f_d 7=F_Fn f_nand (5 :: Int) (6 :: Int);
    f_d 8=F_Fn f_nand (1 :: Int) (1 :: Int);
    f_d 9=F_Fn f_nand (8 :: Int) (6 :: Int);
    f_d 10=F_Fn f_nand (8 :: Int) (7 :: Int);
    f_d 11=F_Fn f_nand (9 :: Int) (12 :: Int);
    f_d 12=F_Fn f_nand (10 :: Int) (11 :: Int);
    f_d a_x=C_NoFn;
    f_benchmark_main a_x=(++) (f_concat (f_map f_showstate (f_findlaststate (f_simulate c_ievents c_istate a_x) c_istate))) "\n";
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
    main = do (n:_) <- getArgs; putStr (f_main (read n :: Int))
}
