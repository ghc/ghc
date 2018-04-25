module Main (main) -- ida
where {
    import System.Environment (getArgs);
--partain: import Fast2haskell;
#include "../Fast2haskell.hs"
    strict_show_i::Int -> [Char];
    strict_show_i x=miraseq x (show x);
    strict_show_d::Double -> [Char];
    strict_show_d x=miraseq x (show x);

data 
    T_pos=F_POS Int Int;
    f_sel_1_POS (F_POS a_int_1 a_int_2)=a_int_1;
    f_sel_2_POS (F_POS a_int_1 a_int_2)=a_int_2;
data 
    T_search=F_SEARCH Int Int T_board [T_pos];
    f_sel_1_SEARCH (F_SEARCH a_int_1 a_int_2 a_board_3 a_po_4)=a_int_1;
    f_sel_2_SEARCH (F_SEARCH a_int_1 a_int_2 a_board_3 a_po_4)=a_int_2;
    f_sel_3_SEARCH (F_SEARCH a_int_1 a_int_2 a_board_3 a_po_4)=a_board_3;
    f_sel_4_SEARCH (F_SEARCH a_int_1 a_int_2 a_board_3 a_po_4)=a_po_4;
data 
    T_solution=F_SOLUTION T_search Int;
    f_sel_1_SOLUTION (F_SOLUTION a_search_1 a_int_2)=a_search_1;
    f_sel_2_SOLUTION (F_SOLUTION a_search_1 a_int_2)=a_int_2;
data 
    T_split t1=F_SPLIT t1 t1;
    f_sel_1_SPLIT (F_SPLIT a_arg_1 a_arg_2)=a_arg_1;
    f_sel_2_SPLIT (F_SPLIT a_arg_1 a_arg_2)=a_arg_2;
    f_sel_frst (F_SPLIT a_s1 a_s2)=a_s1;
data 
    T_square=F_SQUARE T_pos T_pos;
    f_sel_1_SQUARE (F_SQUARE a_pos_1 a_pos_2)=a_pos_1;
    f_sel_2_SQUARE (F_SQUARE a_pos_1 a_pos_2)=a_pos_2;
    f_sel_Curpos (F_SQUARE a_curpos a_home)=a_curpos;
type 
    T_board=T_split [T_square];
data 
    T_conf=F_CONF T_pos [T_square];
    f_sel_1_CONF (F_CONF a_pos_1 a_squar_2)=a_pos_1;
    f_sel_2_CONF (F_CONF a_pos_1 a_squar_2)=a_squar_2;
    f_show_square (F_SQUARE a_ps a_hm)=(:) '(' ((++) (f_show_pos a_ps) ((++) "-" ((++) (f_show_pos a_hm) ")")));
    f_show_pos (F_POS a_x a_y)=(:) '(' ((++) (strict_show_i a_x) ((++) "," ((++) (strict_show_i a_y) ")")));
    f_abs_i a_i1=
        if (((>=) :: (Int -> Int -> Bool)) a_i1 (0 :: Int))
        then a_i1
        else 
            (((negate) :: (Int -> Int)) a_i1);
    f_for a_a a_b a_f=
        if (((>) :: (Int -> Int -> Bool)) a_a a_b)
        then []
        else 
            ((:) (a_f a_a) (f_for (((+) :: (Int -> Int -> Int)) a_a (1 :: Int)) a_b a_f));
    c_ini_NR_SHIFTS=(100 :: Int);
    f_sqr a_x=((*) :: (Int -> Int -> Int)) a_x a_x;
    f_main'::Int -> Int -> Int -> Int -> [Char];
    f_main' a_seed a_size_x a_size_y a_delta=
        let { 
            r_nr_shifts=((+) :: (Int -> Int -> Int)) c_ini_NR_SHIFTS (0 :: Int);
            r_inipos=f_sel_1_CONF r_init_conf;
            r_inisqr=f_sel_2_CONF r_init_conf;
            r_init_conf=f_shuffle (f_goal a_size_x a_size_y) r_nr_shifts (f_cons_rnd_gen a_seed);
            r_sSOLUTION_sSEARCH_bound_depth_board_path_opt_part=f_solve_puzzle r_init_conf a_delta (((+) :: (Int -> Int -> Int)) r_nr_shifts (1 :: Int));
            r_bound=f_sel_1_SEARCH r_sSEARCH_bound_depth_board_path;
            r_sSEARCH_bound_depth_board_path=f_sel_1_SOLUTION r_sSOLUTION_sSEARCH_bound_depth_board_path_opt_part;
            r_opt_part=f_sel_2_SOLUTION r_sSOLUTION_sSEARCH_bound_depth_board_path_opt_part
         } in  (++) (f_show_pos r_inipos) ((++) (f_concat (f_map f_show_square r_inisqr)) ((++) "\n" 
            ((++) (strict_show_i r_bound) ((++) " " ((++) (strict_show_i r_opt_part) "\n")))));
    f_benchmark_main::Int -> [Char];
    f_benchmark_main a_size=f_main' (6 :: Int) (a_size :: Int) (4 :: Int) (7 :: Int);
    f_func_split::(t1 -> Bool) -> [t1] -> T_split [t1];
    f_func_split a_func []=F_SPLIT [] [];
    f_func_split a_func (a_elem_1:a_elem_list)=
        let { 
            r_sSPLIT_true_list_false_list=f_func_split a_func a_elem_list;
            r_true_list=f_sel_1_SPLIT r_sSPLIT_true_list_false_list;
            r_false_list=f_sel_2_SPLIT r_sSPLIT_true_list_false_list
         } in  
            if (a_func a_elem_1)
            then (F_SPLIT ((:) a_elem_1 r_true_list) r_false_list)
            else 
                (F_SPLIT r_true_list ((:) a_elem_1 r_false_list));
    f_func_max::(t1 -> Int) -> t1 -> t1 -> t1;
    f_func_max a_func a_elem_1 a_elem_2=
        if (((<) :: (Int -> Int -> Bool)) (a_func a_elem_1) (a_func a_elem_2))
        then a_elem_2
        else 
            a_elem_1;
    f_rotate::Int -> [t1] -> [t1];
    f_rotate a_nr_rotations []=[];
    f_rotate 0 a_elem_list=a_elem_list;
    f_rotate a_nr_rotations (a_elem_1:a_elem_list)=f_rotate (((-) :: (Int -> Int -> Int)) a_nr_rotations (1 :: Int)) ((++) a_elem_list ((:) a_elem_1 []));
    f_cons_rnd_gen::Int -> [Int];
    f_cons_rnd_gen a_seed=
        let { 
            r_rnd_MULTIPLIER=(4663 :: Int);
            r_rnd_MODULUS=(46199 :: Int)
         } in  f_rnd_val r_rnd_MODULUS r_rnd_MULTIPLIER (f_abs_i a_seed);
    f_rnd_val::Int -> Int -> Int -> [Int];
    f_rnd_val a_md a_mul a_seed=
        let { 
            r_val=((+) :: (Int -> Int -> Int)) (((rem) :: (Int -> Int -> Int)) (((*) :: (Int -> Int -> Int)) a_mul a_seed) a_md) (1 :: Int)
         } in  (:) r_val (f_rnd_val a_md a_mul r_val);
    f_rnd_list::Int -> [t1] -> T_split [t1];
    f_rnd_list a_nr a_rnd_gen=f_take_drop a_nr [] a_rnd_gen;
    f_take_drop::Int -> [t1] -> [t1] -> T_split [t1];
    f_take_drop 0 a_pref a_lst=F_SPLIT a_pref a_lst;
    f_take_drop a_n a_pref (a_h:a_t)=f_take_drop (((-) :: (Int -> Int -> Int)) a_n (1 :: Int)) ((:) a_h a_pref) a_t;
    f_solve_puzzle::T_conf -> Int -> Int -> T_solution;
    f_solve_puzzle (F_CONF a_hole a_disk_list) a_delta a_max_shift=
        let { 
            r_init_manh_dist=f_tot_manh_dist a_disk_list;
            r_worst_case=F_SEARCH a_max_shift (0 :: Int) (F_SPLIT [] []) []
         } in  f_optimize (f_forced_check (F_SEARCH r_init_manh_dist r_init_manh_dist (f_select_moves a_hole a_disk_list) ((:) a_hole []))) r_worst_case a_delta;
    f_divide::T_search -> [T_search];
    f_divide (F_SEARCH a_bound a_depth (F_SPLIT [] a_fixed) a_path)=[];
    f_divide (F_SEARCH a_bound a_depth (F_SPLIT a_movables a_fixed) a_path)=
        let { 
            r_sSQUARE_cur_pos_home=head a_movables;
            r_movable=tail a_movables;
            r_cur_pos=f_sel_1_SQUARE r_sSQUARE_cur_pos_home;
            r_home=f_sel_2_SQUARE r_sSQUARE_cur_pos_home;
            r_hole=head a_path;
            r_sSPLIT_nxt_mov_nxt_fix=f_select_moves r_cur_pos ((++) r_movable a_fixed);
            r_nxt_mov=f_sel_1_SPLIT r_sSPLIT_nxt_mov_nxt_fix;
            r_nxt_fix=f_sel_2_SPLIT r_sSPLIT_nxt_mov_nxt_fix;
            r_delta_manh=((-) :: (Int -> Int -> Int)) (f_manh_dist r_hole r_home) (f_manh_dist r_cur_pos r_home)
         } in  (:) (f_forced_check (F_SEARCH (((+) :: (Int -> Int -> Int)) (((+) :: (Int -> Int -> Int)) a_bound (1 :: Int)) r_delta_manh) (((+) :: (Int -> Int -> Int)) a_depth r_delta_manh) 
            (F_SPLIT r_nxt_mov ((:) (F_SQUARE r_hole r_home) r_nxt_fix)) ((:) r_cur_pos a_path))) (f_divide (F_SEARCH a_bound a_depth (F_SPLIT r_movable 
            ((:) (F_SQUARE r_cur_pos r_home) a_fixed)) a_path));
    f_upper_bound::T_search -> Int;
    f_upper_bound (F_SEARCH a_bound a_depth a_board a_path)=((negate) :: (Int -> Int)) a_bound;
    f_singleton::T_search -> Bool;
    f_singleton (F_SEARCH a_bound a_depth a_board a_path)=((==) :: (Int -> Int -> Bool)) a_depth (0 :: Int);
    f_forced_check::T_search -> T_search;
    f_forced_check a_search_space=
        let { 
            r_depth=f_sel_2_SEARCH a_search_space;
            r_board=f_sel_3_SEARCH a_search_space;
            r_movable=f_sel_1_SPLIT r_board
         } in  
            if (((==) :: (Int -> Int -> Bool)) r_depth (0 :: Int))
            then a_search_space
            else 
            if (null (tail r_movable))
            then (head (f_divide a_search_space))
            else 
                a_search_space;
    f_manh_dist::T_pos -> T_pos -> Int;
    f_manh_dist (F_POS a_a1 a_b1) (F_POS a_a2 a_b2)=((+) :: (Int -> Int -> Int)) (f_abs_i (((-) :: (Int -> Int -> Int)) a_a1 a_a2)) (f_abs_i (((-) :: (Int -> Int -> Int)) a_b1 a_b2));
    f_tot_manh_dist::[T_square] -> Int;
    f_tot_manh_dist []=(0 :: Int);
    f_tot_manh_dist ((F_SQUARE a_cur_pos a_home):a_disk_list)=((+) :: (Int -> Int -> Int)) (f_manh_dist a_cur_pos a_home) (f_tot_manh_dist a_disk_list);
    f_select_moves::T_pos -> [T_square] -> T_board;
    f_select_moves a_hole a_disk_list=f_func_split ((.) (((==) :: (Int -> Int -> Bool)) (1 :: Int)) ((.) (f_manh_dist a_hole) f_sel_Curpos)) a_disk_list;
    f_goal::Int -> Int -> T_conf;
    f_goal a_size_x a_size_y=
        let { 
            r_home_list=tail r_cart_prod;
            r_cart_prod=[F_POS a_x a_y|a_x<-[(1 :: Int)..a_size_x],a_y<-[(1 :: Int)..a_size_y]]
         } in  F_CONF (F_POS (1 :: Int) (1 :: Int)) (f_map f_dup_square r_home_list);
    f_dup_square a_x=F_SQUARE a_x a_x;
    f_shuffle::T_conf -> Int -> [Int] -> T_conf;
    f_shuffle (F_CONF a_hole a_disk_list) a_nr_shift a_rnd_gen=f_rnd_shift a_hole (f_select_moves a_hole a_disk_list) (f_sel_frst (f_rnd_list a_nr_shift a_rnd_gen));
    f_rnd_shift::T_pos -> T_board -> [Int] -> T_conf;
    f_rnd_shift a_hole (F_SPLIT a_movable a_fixed) []=F_CONF a_hole ((++) a_movable a_fixed);
    f_rnd_shift a_hole (F_SPLIT a_movable a_fixed) (a_rnd_1:a_val_list)=
        let { 
            r_sLST_sSQUARE_cur_pos_home_rejected=f_rotate (((rem) :: (Int -> Int -> Int)) a_rnd_1 (length a_movable)) a_movable;
            r_sSQUARE_cur_pos_home=head r_sLST_sSQUARE_cur_pos_home_rejected;
            r_rejected=tail r_sLST_sSQUARE_cur_pos_home_rejected;
            r_cur_pos=f_sel_1_SQUARE r_sSQUARE_cur_pos_home;
            r_home=f_sel_2_SQUARE r_sSQUARE_cur_pos_home
         } in  f_rnd_shift r_cur_pos (f_select_moves r_cur_pos ((:) (F_SQUARE a_hole r_home) ((++) a_fixed r_rejected))) a_val_list;
    f_optimize::T_search -> T_search -> Int -> T_solution;
    f_optimize a_search_space a_worst_case a_delta=f_reppart a_search_space a_worst_case a_delta (F_SOLUTION (f_func_2_max f_upper_bound f_singleton a_search_space a_worst_case) (0 :: Int));
    f_reppart::T_search -> T_search -> Int -> T_solution -> T_solution;
    f_reppart a_search_space a_worst_case a_delta a_cur_sol=
        let { 
            r_cur_opt_space=f_sel_1_SOLUTION a_cur_sol;
            r_nr_part=f_sel_2_SOLUTION a_cur_sol
         } in  
            if (f_singleton r_cur_opt_space)
            then a_cur_sol
            else 
                (f_reppart a_search_space a_worst_case a_delta (f_branch a_delta (f_upper_bound r_cur_opt_space) ((:) a_search_space []) (F_SOLUTION a_worst_case r_nr_part)));
    f_branch::Int -> Int -> [T_search] -> T_solution -> T_solution;
    f_branch a_delta a_cut_off [] a_search_state=a_search_state;
    f_branch a_delta a_cut_off (a_space_1:a_space_list) a_cur_sol=
        let { 
            r_cur_opt_space=f_sel_1_SOLUTION a_cur_sol;
            r_nr_partitions=f_sel_2_SOLUTION a_cur_sol;
            r_upper_bound_1=f_upper_bound a_space_1;
            r_one_more=((+) :: (Int -> Int -> Int)) r_nr_partitions (1 :: Int)
         } in  
            if (((==) :: (Int -> Int -> Bool)) (f_upper_bound r_cur_opt_space) a_cut_off)
            then a_cur_sol
            else 
            if (
                if (f_singleton a_space_1)
                then True
                else 
                    (((<) :: (Int -> Int -> Bool)) r_upper_bound_1 a_cut_off))
            then (f_branch a_delta a_cut_off a_space_list (F_SOLUTION (f_func_2_max f_upper_bound f_singleton r_cur_opt_space a_space_1) r_nr_partitions))
            else 
            if (((>) :: (Int -> Int -> Bool)) (((-) :: (Int -> Int -> Int)) r_upper_bound_1 a_cut_off) a_delta)
            then (miraseq r_one_more (f_branch a_delta a_cut_off ((++) (f_divide a_space_1) a_space_list) (F_SOLUTION r_cur_opt_space r_one_more)))
            else 
                (f_branch a_delta a_cut_off a_space_list (f_par_branch a_cut_off ((:) a_space_1 []) a_cur_sol));
    f_prune::Int -> T_search -> Bool;
    f_prune a_cut_off a_search_space=
        if (f_singleton a_search_space)
        then True
        else 
            (((<) :: (Int -> Int -> Bool)) (f_upper_bound a_search_space) a_cut_off);
    f_par_branch::Int -> [T_search] -> T_solution -> T_solution;
    f_par_branch a_cut_off [] a_search_state=a_search_state;
    f_par_branch a_cut_off (a_space_1:a_space_list) a_cur_sol=
        let { 
            r_cur_opt_space=f_sel_1_SOLUTION a_cur_sol;
            r_nr_partitions=f_sel_2_SOLUTION a_cur_sol;
            r_sSPLIT_pruned_unpruned=f_func_split (f_prune a_cut_off) (f_divide a_space_1);
            r_pruned=f_sel_1_SPLIT r_sSPLIT_pruned_unpruned;
            r_unpruned=f_sel_2_SPLIT r_sSPLIT_pruned_unpruned;
            r_new_opt_space=f_foldl (f_func_2_max f_upper_bound f_singleton) r_cur_opt_space r_pruned;
            r_one_more=((+) :: (Int -> Int -> Int)) r_nr_partitions (1 :: Int)
         } in  
            if (not (null a_space_list))
            then (f_sandwich'' f_collect (F_SANDWARG (f_par_branch a_cut_off ((:) a_space_1 []) (F_SOLUTION r_cur_opt_space (0 :: Int))) (((-) :: (Int -> Int -> Int)) 
                (f_upper_bound a_space_1) a_cut_off)) (F_SANDWARG (f_par_branch a_cut_off a_space_list a_cur_sol) (((-) :: (Int -> Int -> Int)) (f_upper_bound (head a_space_list)) a_cut_off)))
            else 
            if (((==) :: (Int -> Int -> Bool)) (f_upper_bound r_new_opt_space) a_cut_off)
            then (F_SOLUTION r_new_opt_space r_nr_partitions)
            else 
                (miraseq r_one_more (f_par_branch a_cut_off r_unpruned (F_SOLUTION r_new_opt_space r_one_more)));
    f_collect::T_solution -> T_solution -> T_solution;
    f_collect (F_SOLUTION a_sp1 a_nr1) (F_SOLUTION a_sp2 a_nr2)=F_SOLUTION (f_func_2_max f_upper_bound f_singleton a_sp1 a_sp2) (((+) :: (Int -> Int -> Int)) a_nr1 a_nr2);
    f_func_2_max::(T_search -> Int) -> (T_search -> Bool) -> T_search -> T_search -> T_search;
    f_func_2_max a_upper_bound a_singleton a_space_1 a_space_2=
        if (((/=) :: (Int -> Int -> Bool)) (a_upper_bound a_space_1) (a_upper_bound a_space_2))
        then (f_func_max a_upper_bound a_space_1 a_space_2)
        else 
        if (a_singleton a_space_1)
        then a_space_1
        else 
            a_space_2;
data 
    T_sandwarg t1=F_SANDWARG t1 Int;
    f_force' a_x=a_x;
    f_sandwich''::(t1 -> t2 -> t3) -> (T_sandwarg t1) -> (T_sandwarg t2) -> t3;
    f_sandwich'' a_f (F_SANDWARG a_vap1 a_gsd1) (F_SANDWARG a_vap2 a_gsd2)=f_force' (miraseq (f_force' (miraseq (f_force' a_vap1) a_vap2)) (a_f a_vap1 a_vap2));
    f_sandwich'::(t1 -> t2) -> (T_sandwarg t1) -> t2;
    f_sandwich' a_f (F_SANDWARG a_vap1 a_gsd1)=f_force' (miraseq (f_force' a_vap1) (a_f a_vap1));
    f_prenorm::t1 -> t2 -> t2;
    f_prenorm a_a a_b=miraseq (f_force' a_a) a_b;
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
