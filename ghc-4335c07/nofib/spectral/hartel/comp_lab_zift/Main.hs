module Main (main) -- comp_lab_zift
where {
    import System.Environment (getArgs);
--partain: import Fast2haskell;
#include "../Fast2haskell.hs"
    strict_show_i::Int -> [Char];
    strict_show_i x=miraseq x (show x);
    strict_show_d::Double -> [Char];
    strict_show_d x=miraseq x (show x);

    f_union_br::[T_lab_range] -> [T_lab_range] -> T_object_set -> T_object_set;
    f_union_br [] a_lst a_obj=a_obj;
    f_union_br a_lst [] a_obj=a_obj;
    f_union_br a_left_range a_right_range a_obj=
        let { 
            r_united_obj=f_union_obj a_obj (r_find__obj r_ol) (r_find__obj r_or);
            r_find__obj=f_find a_obj;
            r_sLAB_RANGE_ol_sRANGE_bl_wl=head a_left_range;
            r_rl=tail a_left_range;
            r_bl=f_sel_1_RANGE r_sRANGE_bl_wl;
            r_wl=f_sel_2_RANGE r_sRANGE_bl_wl;
            r_ol=f_sel_1_LAB_RANGE r_sLAB_RANGE_ol_sRANGE_bl_wl;
            r_sRANGE_bl_wl=f_sel_2_LAB_RANGE r_sLAB_RANGE_ol_sRANGE_bl_wl;
            r_sLAB_RANGE_or_sRANGE_br_wr=head a_right_range;
            r_rr=tail a_right_range;
            r_br=f_sel_1_RANGE r_sRANGE_br_wr;
            r_wr=f_sel_2_RANGE r_sRANGE_br_wr;
            r_or=f_sel_1_LAB_RANGE r_sLAB_RANGE_or_sRANGE_br_wr;
            r_sRANGE_br_wr=f_sel_2_LAB_RANGE r_sLAB_RANGE_or_sRANGE_br_wr
         } in  
            if (((<=) :: (Int -> Int -> Bool)) r_wl r_br)
            then (f_union_br r_rl a_right_range a_obj)
            else 
            if (((<=) :: (Int -> Int -> Bool)) r_wr r_bl)
            then (f_union_br a_left_range r_rr a_obj)
            else 
            if (((<=) :: (Int -> Int -> Bool)) r_wr r_wl)
            then (f_union_br a_left_range r_rr r_united_obj)
            else 
                (f_union_br r_rl a_right_range r_united_obj);
    f_assign_row::[T_range] -> T_gen -> T_labrow;
    f_assign_row [] a_buy_labels=F_LABROW [] [];
    f_assign_row (a_rnge:a_rest) a_buy_labels=
        let { 
            r_sLABROW_rnew_objnew=f_assign_row a_rest r_rest_buys;
            r_rnew=f_sel_1_LABROW r_sLABROW_rnew_objnew;
            r_objnew=f_sel_2_LABROW r_sLABROW_rnew_objnew;
            r_sLABGENS_new_label_rest_buys=f_create_label a_buy_labels;
            r_new_label=f_sel_1_LABGENS r_sLABGENS_new_label_rest_buys;
            r_rest_buys=f_sel_2_LABGENS r_sLABGENS_new_label_rest_buys;
            r_new_object=f_singleton r_new_label
         } in  F_LABROW ((:) (F_LAB_RANGE r_new_label a_rnge) r_rnew) ((:) r_new_object r_objnew);
    f_union_fields::T_field -> T_field -> T_field;
    f_union_fields (F_FIELD a_ll a_lr a_lobj a_lpix) (F_FIELD a_rl a_rr a_robj a_rpix)=
        let { 
            r_in_l=
                if (null a_lr)
                then a_ll
                else 
                    a_lr;
            r_out_r=
                if (null a_rr)
                then a_rl
                else 
                    a_rr;
            r_new_obj=f_union_br r_in_l a_rl (f_combine a_lobj a_robj);
            r_l_out_lab=f_map (f_ap_label r_new_obj) a_ll;
            r_r_out_lab=f_map (f_ap_label r_new_obj) r_out_r
         } in  F_FIELD r_l_out_lab r_r_out_lab r_new_obj ((++) a_lpix a_rpix);
    f_pix2field::[T_lenrow] -> Int -> T_gen -> Int -> T_field;
    f_pix2field ((F_LENROW a_lrow a_row):[]) a_len a_buy_labels a_threshold=
        let { 
            r_sLABROW_found_labels_used_obj=f_assign_row a_row a_buy_labels;
            r_found_labels=f_sel_1_LABROW r_sLABROW_found_labels_used_obj;
            r_used_obj=f_sel_2_LABROW r_sLABROW_found_labels_used_obj;
            r_new_labels=(:) (F_LNLBRAN a_lrow r_found_labels) []
         } in  F_FIELD r_found_labels [] r_used_obj r_new_labels;
    f_pix2field a_pixels a_len a_buy_labels a_threshold=
        let { 
            (r_left_pixels,r_right_pixels)=f_split a_pixels r_len_l;
            r_left_field=f_pix2field r_left_pixels r_len_l r_left_buy a_threshold;
            r_right_field=f_pix2field r_right_pixels r_len_r r_right_buy a_threshold;
            r_left_field_seq=f_pix2field_seq r_left_pixels r_len_l r_left_buy;
            r_right_field_seq=f_pix2field_seq r_right_pixels r_len_r r_right_buy;
            r_left_sum=f_sum (f_map f_sel_len r_left_pixels);
            r_right_sum=f_sum (f_map f_sel_len r_right_pixels);
            (r_left_buy,r_right_buy)=f_split_buy a_buy_labels r_len_l;
            r_len_l=((quot) :: (Int -> Int -> Int)) a_len (2 :: Int);
            r_len_r=((-) :: (Int -> Int -> Int)) a_len r_len_l
         } in  
            if (
                if (((>) :: (Int -> Int -> Bool)) r_left_sum a_threshold)
                then (((>) :: (Int -> Int -> Bool)) r_right_sum a_threshold)
                else 
                    False)
            then (f_par_pix2field f_union_fields r_left_field r_left_sum r_right_field r_right_sum)
            else 
            if (((>) :: (Int -> Int -> Bool)) r_right_sum a_threshold)
            then (f_union_fields r_left_field_seq r_right_field)
            else 
            if (((>) :: (Int -> Int -> Bool)) r_left_sum a_threshold)
            then (f_union_fields r_left_field r_right_field_seq)
            else 
                (f_pix2field_seq a_pixels a_len a_buy_labels);
    f_par_pix2field a_union_fields a_left_field a_left_sum a_right_field a_right_sum=f_sandwich'' a_union_fields (F_SANDWARG a_left_field (((*) :: (Int -> Int -> Int)) (2 :: Int) a_left_sum)) (F_SANDWARG a_right_field (((*) :: (Int -> Int -> Int)) (2 :: Int) a_right_sum));
    f_pix2field_seq::[T_lenrow] -> Int -> T_gen -> T_field;
    f_pix2field_seq ((F_LENROW a_lrow a_row):[]) a_len a_buy_labels=
        let { 
            r_sLABROW_found_labels_used_obj=f_assign_row a_row a_buy_labels;
            r_found_labels=f_sel_1_LABROW r_sLABROW_found_labels_used_obj;
            r_used_obj=f_sel_2_LABROW r_sLABROW_found_labels_used_obj;
            r_new_labels=(:) (F_LNLBRAN a_lrow r_found_labels) []
         } in  F_FIELD r_found_labels [] r_used_obj r_new_labels;
    f_pix2field_seq a_pixels a_len a_buy_labels=
        let { 
            (r_left_pixels,r_right_pixels)=f_split a_pixels r_len_l;
            (r_left_buy,r_right_buy)=f_split_buy a_buy_labels r_len_l;
            r_len_l=((quot) :: (Int -> Int -> Int)) a_len (2 :: Int);
            r_len_r=((-) :: (Int -> Int -> Int)) a_len r_len_l
         } in  f_union_fields (f_pix2field_seq r_left_pixels r_len_l r_left_buy) (f_pix2field_seq r_right_pixels r_len_r r_right_buy);
    f_map_label::(T_gen -> Int -> T_object_set -> T_object_set) -> [T_lnlbran] -> Int -> T_gen -> T_object_set -> Int -> [[T_lab_range]];
    f_map_label a_zift_l ((F_LNLBRAN a_lrow a_row):[]) a_len a_buy_labels a_obj a_label_threshold=(:) (f_map (f_ap_label a_obj) a_row) [];
    f_map_label a_zift_l a_labels a_len a_buy_labels a_obj a_label_threshold=
        let { 
            (r_left_labels,r_right_labels)=f_split a_labels r_len_l;
            r_left_image=f_map_label a_zift_l r_left_labels r_len_l r_left_buy r_left_obj a_label_threshold;
            r_right_image=f_map_label a_zift_l r_right_labels r_len_r r_right_buy r_right_obj a_label_threshold;
            r_left_image_seq=f_map_label_seq r_left_labels r_left_obj;
            r_right_image_seq=f_map_label_seq r_right_labels r_right_obj;
            r_left_sum=f_sum (f_map f_sel_lenb r_left_labels);
            r_right_sum=f_sum (f_map f_sel_lenb r_right_labels);
            (r_left_buy,r_right_buy)=f_split_buy a_buy_labels r_len_l;
            r_left_obj=a_zift_l r_left_buy r_len_l a_obj;
            r_right_obj=a_zift_l r_right_buy r_len_r a_obj;
            r_len_l=((quot) :: (Int -> Int -> Int)) a_len (2 :: Int);
            r_len_r=((-) :: (Int -> Int -> Int)) a_len r_len_l
         } in  
            if (
                if (((>) :: (Int -> Int -> Bool)) r_left_sum a_label_threshold)
                then (((>) :: (Int -> Int -> Bool)) r_right_sum a_label_threshold)
                else 
                    False)
            then (f_par_map_label (++) r_left_image r_left_sum r_right_image r_right_sum)
            else 
            if (((>) :: (Int -> Int -> Bool)) r_left_sum a_label_threshold)
            then ((++) r_left_image r_right_image_seq)
            else 
            if (((>) :: (Int -> Int -> Bool)) r_right_sum a_label_threshold)
            then ((++) r_left_image_seq r_right_image)
            else 
                (f_map_label_seq a_labels a_obj);
    f_par_map_label a_join a_left_image a_left_sum a_right_image a_right_sum=f_sandwich'' a_join (F_SANDWARG a_left_image (((-) :: (Int -> Int -> Int)) (((*) :: (Int -> Int -> Int)) (2 :: Int) a_left_sum) (1 :: Int))) (F_SANDWARG a_right_image (((-) :: (Int -> Int -> Int)) 
        (((*) :: (Int -> Int -> Int)) (2 :: Int) a_right_sum) (1 :: Int)));
    f_map_label_seq::[T_lnlbran] -> T_object_set -> [[T_lab_range]];
    f_map_label_seq a_labels a_obj=
        let { 
            f_sel_range (F_LNLBRAN a_l a_ran)=a_ran
         } in  f_map (f_map (f_ap_label a_obj)) (f_map f_sel_range a_labels);
    f_comp_lab_diff::(T_gen -> Int -> T_object_set -> T_object_set) -> Int -> [T_lenrow] -> Int -> [[T_lab_range]];
    f_comp_lab_diff a_zift_l a_len a_pixels a_threshold=
        let { 
            r_sFIELD_leftb_rightb_obj_table_labels=f_pix2field a_pixels r_p_len r_gen0 a_threshold;
            r_obj_table=f_sel_3_FIELD r_sFIELD_leftb_rightb_obj_table_labels;
            r_labels=f_sel_4_FIELD r_sFIELD_leftb_rightb_obj_table_labels;
            r_p_len=length a_pixels;
            r_gen0=F_GEN a_len (0 :: Int) (0 :: Int)
         } in  f_map_label a_zift_l r_labels r_p_len r_gen0 r_obj_table a_threshold;
    f_comp_lab_diff'::(T_gen -> Int -> T_object_set -> T_object_set) -> Int -> [T_lenrow] -> Int -> ([[T_lab_range]],[T_lnlbran],T_object_set);
    f_comp_lab_diff' a_zift_l a_len a_pixels a_threshold=
        let { 
            r_sFIELD_leftb_rightb_obj_table_labels=f_pix2field a_pixels r_p_len r_gen0 a_threshold;
            r_obj_table=f_sel_3_FIELD r_sFIELD_leftb_rightb_obj_table_labels;
            r_labels=f_sel_4_FIELD r_sFIELD_leftb_rightb_obj_table_labels;
            r_p_len=length a_pixels;
            r_gen0=F_GEN a_len (0 :: Int) (0 :: Int)
         } in  (f_map_label a_zift_l r_labels r_p_len r_gen0 r_obj_table a_threshold,r_labels,r_obj_table);
    f_split::[t1] -> Int -> ([t1],[t1]);
    f_split a_lst a_n=f_takedrop a_n a_lst;
    f_show_row a_lst=f_concat (f_map ((.) ((:) ' ') strict_show_i) a_lst);
    f_show_mat a_mat=f_concat (f_map ((.) ((:) '\o012') f_show_row) a_mat);
    f_abs_i a_i=
        if (((>=) :: (Int -> Int -> Bool)) a_i (0 :: Int))
        then a_i
        else 
            (((negate) :: (Int -> Int)) a_i);
    f_main_field'::[[T_pixel]] -> Int -> Int -> [[T_lab_range]];
    f_main_field' a_figure a_ntimes a_threshold=
        let { 
            r_figure_l=length (head r_the_figure);
            r_image=f_map f_pix2range r_the_figure;
            r_the_figure=f_n_fig (f_abs_i a_ntimes) a_figure
         } in  
            if (((>=) :: (Int -> Int -> Bool)) a_ntimes (0 :: Int))
            then (f_prenorm r_image (f_comp_lab_diff f_zift_l r_figure_l r_image a_threshold))
            else 
                (f_prenorm r_image (f_comp_lab_diff f_no_zift r_figure_l r_image a_threshold));
    f_main_shown' a_figure a_ntimes a_threshold=
        let { 
            r_fig_len=length (head r_the_figure);
            r_the_figure=f_n_fig a_ntimes a_figure
         } in  f_show_mat (f_map (f_field2pix r_fig_len (0 :: Int)) (f_main_field' a_figure a_ntimes a_threshold));
    f_main' a_sel_fig a_ntimes a_threshold=
        let { 
            r_the_fig=
                if (((==) :: (Int -> Int -> Bool)) a_sel_fig (0 :: Int))
                then c_mini_figure
                else 
                if (((==) :: (Int -> Int -> Bool)) a_sel_fig (1 :: Int))
                then c_small_figure
                else 
                if (((==) :: (Int -> Int -> Bool)) a_sel_fig (2 :: Int))
                then c_figure
                else 
                if (((==) :: (Int -> Int -> Bool)) a_sel_fig (3 :: Int))
                then c_figure_l
                else 
                if (((==) :: (Int -> Int -> Bool)) a_sel_fig (4 :: Int))
                then c_figure_l_l
                else 
                    c_figure_l
         } in  strict_show_i (length (f_main_field' r_the_fig a_ntimes a_threshold));
    f_main'' a_input=
        let { 
            r_tot_len=f_sum (f_map length a_input)
         } in  f_main_shown' a_input (1 :: Int) r_tot_len;
    f_show_all_lab::([[T_lab_range]],[T_lnlbran],T_object_set) -> Int -> [Char];
    f_show_all_lab (a_pxls,a_lbls,a_objs) a_fig_len=(++) "\ncomponent image:" ((++) (f_show_mat (f_map (f_field2pix a_fig_len (0 :: Int)) a_pxls)) ((++) "\n\nlab-ranges:" 
        ((++) (f_concat (f_map ((.) ((:) '\o012') f_show_lnlbran) a_lbls)) ((++) "\n\nobject-sets:" ((++) 
        (f_concat (f_map ((.) ((:) '\o012') (f_showset strict_show_i)) a_objs)) "\n\n")))));
    f_main_shown a_figure a_ntimes a_threshold=
        let { 
            r_fig_len=length (head r_the_figure);
            r_image=f_map f_pix2range r_the_figure;
            r_the_figure=f_n_fig a_ntimes a_figure
         } in  f_show_all_lab (f_comp_lab_diff' f_zift_l r_fig_len r_image a_threshold) r_fig_len;
    f_benchmark_main a_ntimes=
        let { 
            r_tot_len=((*) :: (Int -> Int -> Int)) a_ntimes (f_sum (f_map length c_figure_l))
         } in  (++) (f_sumcode (f_main_shown c_figure_l a_ntimes r_tot_len)) "\n";
    f_sumcode::[Char] -> [Char];
    f_sumcode a_xs=
        let { 
            f_sumcode' [] a_sum a_n=(++) (strict_show_i (((+) :: (Int -> Int -> Int)) a_sum a_n)) ((:) '/' (strict_show_i a_n));
            f_sumcode' (a_x:a_xs) a_sum a_n=f_sumcode' a_xs (((+) :: (Int -> Int -> Int)) a_sum (fromEnum a_x)) (((+) :: (Int -> Int -> Int)) a_n (1 :: Int))
         } in  f_sumcode' a_xs (0 :: Int) (0 :: Int);
data 
    T_sandwarg t1=F_SANDWARG t1 Int;
    f_force' a_x=a_x;
    f_sandwich''::(t1 -> t2 -> t3) -> (T_sandwarg t1) -> (T_sandwarg t2) -> t3;
    f_sandwich'' a_f (F_SANDWARG a_vap1 a_gsd1) (F_SANDWARG a_vap2 a_gsd2)=f_force' (miraseq (f_force' (miraseq (f_force' a_vap1) a_vap2)) (a_f a_vap1 a_vap2));
    f_sandwich'::(t1 -> t2) -> (T_sandwarg t1) -> t2;
    f_sandwich' a_f (F_SANDWARG a_vap1 a_gsd1)=f_force' (miraseq (f_force' a_vap1) (a_f a_vap1));
    f_prenorm::t1 -> t2 -> t2;
    f_prenorm a_a a_b=miraseq (f_force' a_a) a_b;
data 
    T_splitter t1=F_SPLIT t1 t1;
    f_sel_1_SPLIT (F_SPLIT a_arg_1 a_arg_2)=a_arg_1;
    f_sel_2_SPLIT (F_SPLIT a_arg_1 a_arg_2)=a_arg_2;
    f_takedrop::Int -> [t1] -> ([t1],[t1]);
    f_takedrop 0 a_lst=([],a_lst);
    f_takedrop a_n (a_h:a_t)=
        let { 
            (r_ta,r_dr)=f_takedrop (((-) :: (Int -> Int -> Int)) a_n (1 :: Int)) a_t
         } in  ((:) a_h r_ta,r_dr);
    f_xcount::Int -> Int -> [Int];
    f_xcount a_a a_b=
        if (((<) :: (Int -> Int -> Bool)) a_a a_b)
        then ((:) a_a (f_xcount (((+) :: (Int -> Int -> Int)) a_a (1 :: Int)) a_b))
        else 
            [];
type 
    T_label=Int;
    f_cmp_label::T_label -> T_label -> Int;
    f_cmp_label a_x a_y=((-) :: (Int -> Int -> Int)) a_x a_y;
data 
    T_gen=F_GEN Int Int Int;
    f_sel_1_GEN (F_GEN a_int_1 a_int_2 a_int_3)=a_int_1;
    f_sel_2_GEN (F_GEN a_int_1 a_int_2 a_int_3)=a_int_2;
    f_sel_3_GEN (F_GEN a_int_1 a_int_2 a_int_3)=a_int_3;
data 
    T_labgens=F_LABGENS T_label T_gen;
    f_sel_1_LABGENS (F_LABGENS a_label_1 a_gen_2)=a_label_1;
    f_sel_2_LABGENS (F_LABGENS a_label_1 a_gen_2)=a_gen_2;
    c_no_label=((negate) :: (Int -> Int)) (1 :: Int);
    f_split_buy::T_gen -> Int -> (T_gen,T_gen);
    f_split_buy (F_GEN a_row_len a_my_row a_col) a_new_len=(F_GEN a_row_len a_my_row a_col,F_GEN a_row_len (((+) :: (Int -> Int -> Int)) a_my_row a_new_len) a_col);
    f_create_label::T_gen -> T_labgens;
    f_create_label (F_GEN a_row_len a_my_row a_col)=F_LABGENS (f_unique (((+) :: (Int -> Int -> Int)) (((*) :: (Int -> Int -> Int)) a_row_len a_my_row) a_col)) (F_GEN a_row_len a_my_row (((+) :: (Int -> Int -> Int)) a_col (1 :: Int)));
    f_unique::Int -> T_label;
    f_unique a_lab=a_lab;
data 
    T_pixel=C_Black | C_White;
    c_bL=C_Black;
    c_w=C_White;
    f_is_black C_Black=True;
    f_is_black a_any=False;
    f_is_white C_White=True;
    f_is_white a_any=False;
data 
    T_range=F_RANGE Int Int;
    f_sel_1_RANGE (F_RANGE a_int_1 a_int_2)=a_int_1;
    f_sel_2_RANGE (F_RANGE a_int_1 a_int_2)=a_int_2;
data 
    T_lenrow=F_LENROW Int [T_range];
    f_sel_1_LENROW (F_LENROW a_int_1 a_rang_2)=a_int_1;
    f_sel_2_LENROW (F_LENROW a_int_1 a_rang_2)=a_rang_2;
    f_sel_len (F_LENROW a_n a_r)=a_n;
    f_pix2range::[T_pixel] -> T_lenrow;
    f_pix2range a_lst=
        let { 
            r_ranges=f_find_black a_lst (0 :: Int)
         } in  F_LENROW (length r_ranges) r_ranges;
    f_find_black::[T_pixel] -> Int -> [T_range];
    f_find_black [] a_count=[];
    f_find_black (C_White:a_rest) a_count=f_find_black a_rest (((+) :: (Int -> Int -> Int)) a_count (1 :: Int));
    f_find_black (a_bl_pix:a_rest) a_count=
        let { 
            (r_top_white,r_after_white)=f_find_white a_rest (((+) :: (Int -> Int -> Int)) a_count (1 :: Int))
         } in  (:) (F_RANGE a_count r_top_white) (f_find_black r_after_white (((+) :: (Int -> Int -> Int)) r_top_white (1 :: Int)));
    f_find_white::[T_pixel] -> Int -> (Int,[T_pixel]);
    f_find_white [] a_count=(a_count,[]);
    f_find_white (C_Black:a_rest) a_count=f_find_white a_rest (((+) :: (Int -> Int -> Int)) a_count (1 :: Int));
    f_find_white (a_wh_pix:a_rest) a_count=(a_count,a_rest);
    f_show_pix::T_pixel -> [Char];
    f_show_pix C_Black="B ";
    f_show_pix a_any="  ";
    f_show_image::[[T_pixel]] -> [Char];
    f_show_image a_image=f_concat (f_map ((.) ((:) '\o012') ((.) f_concat (f_map f_show_pix))) a_image);
    f_show_range::T_range -> [Char];
    f_show_range (F_RANGE a_sob a_sow)=(++) "[" ((++) (strict_show_i a_sob) ((++) ":" ((++) (strict_show_i a_sow) "]")));
data 
    T_tree t1=F_LEAF t1 | F_TREE t1 (T_tree t1) (T_tree t1) | C_ERROR_TREE;
    f_sel_1_TREE (F_TREE a_arg_1 a_tree_2 a_tree_3)=a_arg_1;
    f_sel_2_TREE (F_TREE a_arg_1 a_tree_2 a_tree_3)=a_tree_2;
    f_sel_3_TREE (F_TREE a_arg_1 a_tree_2 a_tree_3)=a_tree_3;
    f_sel_1_LEAF (F_LEAF a_arg_1)=a_arg_1;
data 
    T_lab_tree t1=C_NilLt | F_LAB_TREE t1 (T_tree t1) | C_ERROR_LAB_TREE;
    f_sel_1_LAB_TREE (F_LAB_TREE a_arg_1 a_tree_2)=a_arg_1;
    f_sel_2_LAB_TREE (F_LAB_TREE a_arg_1 a_tree_2)=a_tree_2;
type 
    T_set t1=T_lab_tree t1;
    f_top (F_LAB_TREE a_repr a_tr)=a_repr;
    f_singleton a_x=F_LAB_TREE a_x (f_single_tree a_x);
    f_set_eq a_cmp C_NilLt C_NilLt=True;
    f_set_eq a_cmp C_NilLt a_any=False;
    f_set_eq a_cmp a_any C_NilLt=False;
    f_set_eq a_cmp (F_LAB_TREE a_l1 a_t1) (F_LAB_TREE a_l2 a_t2)=((==) :: (Int -> Int -> Bool)) (a_cmp a_l1 a_l2) c_cmp_equal;
    c_empty_set=C_NilLt;
    f_is_empty C_NilLt=True;
    f_is_empty a_any=False;
    f_is_member a_cmp (F_LAB_TREE a_lbl a_tre) a_p=f_tree_member a_cmp a_tre a_p;
    f_my_union a_cmp (F_LAB_TREE a_lbl_l a_left) (F_LAB_TREE a_lbl_r a_right)=F_LAB_TREE a_lbl_l (f_tree_union a_cmp a_left a_right);
    f_select_and_label a_cmp a_min a_lub (F_LAB_TREE a_lbl a_tre)=F_LAB_TREE a_lbl (f_tree_sel a_cmp a_min a_lub a_tre);
    f_showset a_f C_NilLt="()";
    f_showset a_f (F_LAB_TREE a_lbl (F_LEAF a_key))=(++) "{" ((++) (a_f a_lbl) ((++) " " ((++) (a_f a_key) "}")));
    f_showset a_f (F_LAB_TREE a_lbl (F_TREE a_key a_lft a_rght))=(++) "{" ((++) (a_f a_lbl) ((++) " " ((++) (a_f a_key) 
        ((++) " " ((++) (f_showtree a_f a_lft) ((++) " " ((++) (f_showtree a_f a_rght) "}")))))));
    f_single_tree::t1 -> T_tree t1;
    f_single_tree a_x=F_LEAF a_x;
    f_tree_member::(t1 -> t1 -> Int) -> (T_tree t1) -> t1 -> Bool;
    f_tree_member a_cmp (F_LEAF a_key) a_p=((==) :: (Int -> Int -> Bool)) (a_cmp a_p a_key) c_cmp_equal;
    f_tree_member a_cmp (F_TREE a_key a_left a_right) a_p=
        if (((>) :: (Int -> Int -> Bool)) (a_cmp a_p a_key) c_cmp_equal)
        then (f_tree_member a_cmp a_right a_p)
        else 
            (f_tree_member a_cmp a_left a_p);
    f_t_max::(T_tree t1) -> t1;
    f_t_max (F_LEAF a_key)=a_key;
    f_t_max (F_TREE a_key a_left a_right)=f_t_max a_right;
    f_t_min::(T_tree t1) -> t1;
    f_t_min (F_LEAF a_key)=a_key;
    f_t_min (F_TREE a_key a_left a_right)=f_t_min a_left;
    f_flatten::(T_tree t1) -> [t1];
    f_flatten (F_LEAF a_key)=(:) a_key [];
    f_flatten (F_TREE a_key a_left a_right)=(++) (f_flatten a_left) (f_flatten a_right);
    f_make_tree::Int -> [t1] -> T_lab_tree t1;
    f_make_tree a_n (a_h:[])=F_LAB_TREE a_h (f_single_tree a_h);
    f_make_tree a_n a_lst=
        let { 
            r_sLAB_TREE_nl_l_tree=f_make_tree r_n_2 r_l_list;
            r_nl=f_sel_1_LAB_TREE r_sLAB_TREE_nl_l_tree;
            r_l_tree=f_sel_2_LAB_TREE r_sLAB_TREE_nl_l_tree;
            r_sLAB_TREE_nr_r_tree=f_make_tree (((-) :: (Int -> Int -> Int)) a_n r_n_2) r_r_list;
            r_nr=f_sel_1_LAB_TREE r_sLAB_TREE_nr_r_tree;
            r_r_tree=f_sel_2_LAB_TREE r_sLAB_TREE_nr_r_tree;
            r_n_2=((quot) :: (Int -> Int -> Int)) a_n (2 :: Int);
            (r_l_list,r_r_list)=f_takedrop r_n_2 a_lst
         } in  F_LAB_TREE r_nr (F_TREE r_nl r_l_tree r_r_tree);
    f_tree_union::(t1 -> t1 -> Int) -> (T_tree t1) -> (T_tree t1) -> T_tree t1;
    f_tree_union a_cmp a_left a_right=
        let { 
            r_sLAB_TREE_k_tr=f_make_tree r_n r_flat_list;
            r_tr=f_sel_2_LAB_TREE r_sLAB_TREE_k_tr;
            r_max_left=f_t_max a_left;
            r_flat_list=f_merge_cmp a_cmp (f_flatten a_left) (f_flatten a_right);
            r_n=length r_flat_list
         } in  
            if (((<) :: (Int -> Int -> Bool)) (a_cmp r_max_left (f_t_min a_right)) c_cmp_equal)
            then (F_TREE r_max_left a_left a_right)
            else 
                r_tr;
    f_tree_sel::(t1 -> t1 -> Int) -> t1 -> t1 -> (T_tree t1) -> T_tree t1;
    f_tree_sel a_cmp a_min a_lub a_tre=
        let { 
            r_sLAB_TREE_k_tr=f_my_sel a_tre;
            r_tr=f_sel_2_LAB_TREE r_sLAB_TREE_k_tr;
            f_my_sel (F_LEAF a_key)=F_LAB_TREE a_key (F_LEAF a_key);
            f_my_sel (F_TREE a_key a_left a_right)=
                let { 
                    r_sLAB_TREE_max_l_lt=f_my_sel a_left;
                    r_max_l=f_sel_1_LAB_TREE r_sLAB_TREE_max_l_lt;
                    r_lt=f_sel_2_LAB_TREE r_sLAB_TREE_max_l_lt;
                    r_sLAB_TREE_max_r_rt=f_my_sel a_right;
                    r_max_r=f_sel_1_LAB_TREE r_sLAB_TREE_max_r_rt;
                    r_rt=f_sel_2_LAB_TREE r_sLAB_TREE_max_r_rt
                 } in  
                    if (((>) :: (Int -> Int -> Bool)) (a_cmp a_min a_key) c_cmp_equal)
                    then (f_my_sel a_right)
                    else 
                    if (((<=) :: (Int -> Int -> Bool)) (a_cmp a_lub (f_t_min a_right)) c_cmp_equal)
                    then (f_my_sel a_left)
                    else 
                        (F_LAB_TREE r_max_r (F_TREE r_max_l r_lt r_rt))
         } in  r_tr;
    f_showtree::(t1 -> [Char]) -> (T_tree t1) -> [Char];
    f_showtree a_f (F_LEAF a_key)=(++) "[" ((++) (a_f a_key) "()]");
    f_showtree a_f (F_TREE a_key a_lft a_rght)=(++) "[" ((++) (a_f a_key) ((++) (f_showtree a_f a_lft) ((++) 
        (f_showtree a_f a_rght) "]")));
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
type 
    T_object=T_set T_label;
    c_no_object=f_id c_empty_set;
type 
    T_object_set=[T_object];
    f_zift_l::T_gen -> Int -> T_object_set -> T_object_set;
    f_zift_l (F_GEN a_rl a_start a_c) a_len a_labels=f_filter_set (((*) :: (Int -> Int -> Int)) a_start a_rl) (((*) :: (Int -> Int -> Int)) (((+) :: (Int -> Int -> Int)) a_start a_len) a_rl) a_labels;
    f_no_zift::T_gen -> Int -> T_object_set -> T_object_set;
    f_no_zift a_gen a_len a_labels=a_labels;
    f_extract::(t1 -> t1 -> Int) -> (T_set t1) -> [(T_set t1)] -> [(T_set t1)];
    f_extract a_cmp a_o1 []=[];
    f_extract a_cmp a_o1 (a_h:a_t)=
        if (f_set_eq a_cmp a_o1 a_h)
        then a_t
        else 
            ((:) a_h (f_extract a_cmp a_o1 a_t));
    f_extract_2::(t1 -> t1 -> Int) -> (T_set t1) -> (T_set t1) -> [(T_set t1)] -> [(T_set t1)];
    f_extract_2 a_cmp a_o1 a_o2 []=[];
    f_extract_2 a_cmp a_o1 a_o2 (a_h:a_t)=
        if (f_set_eq a_cmp a_o1 a_h)
        then (f_extract a_cmp a_o2 a_t)
        else 
        if (f_set_eq a_cmp a_o2 a_h)
        then (f_extract a_cmp a_o1 a_t)
        else 
            ((:) a_h (f_extract_2 a_cmp a_o1 a_o2 a_t));
    f_find::T_object_set -> T_label -> T_object;
    f_find a_sset a_p=
        if (((==) :: (Int -> Int -> Bool)) a_p c_no_label)
        then c_no_object
        else 
            (f_find_OK a_sset a_p);
    f_find_OK::T_object_set -> T_label -> T_object;
    f_find_OK [] a_p=c_no_object;
    f_find_OK (a_h:a_t) a_p=
        if (f_is_member f_cmp_label a_h a_p)
        then a_h
        else 
            (f_find_OK a_t a_p);
    f_union_obj::T_object_set -> T_object -> T_object -> T_object_set;
    f_union_obj a_obj a_o1 a_o2=
        if (f_set_eq f_cmp_label a_o1 a_o2)
        then a_obj
        else 
            ((:) (f_my_union f_cmp_label a_o1 a_o2) (f_extract_2 f_cmp_label a_o1 a_o2 a_obj));
    f_combine::[T_object] -> [T_object] -> [T_object];
    f_combine a_set1 a_set2=(++) a_set1 a_set2;
    f_filter_set::T_label -> T_label -> T_object_set -> T_object_set;
    f_filter_set a_min a_lub a_obj=f_map (f_select_and_label f_cmp_label a_min a_lub) a_obj;
data 
    T_lab_range=F_LAB_RANGE T_label T_range;
    f_sel_1_LAB_RANGE (F_LAB_RANGE a_label_1 a_range_2)=a_label_1;
    f_sel_2_LAB_RANGE (F_LAB_RANGE a_label_1 a_range_2)=a_range_2;
data 
    T_field=F_FIELD [T_lab_range] [T_lab_range] T_object_set [T_lnlbran];
    f_sel_1_FIELD (F_FIELD a_lab_rang_1 a_lab_rang_2 a_object_set_3 a_lnlbra_4)=a_lab_rang_1;
    f_sel_2_FIELD (F_FIELD a_lab_rang_1 a_lab_rang_2 a_object_set_3 a_lnlbra_4)=a_lab_rang_2;
    f_sel_3_FIELD (F_FIELD a_lab_rang_1 a_lab_rang_2 a_object_set_3 a_lnlbra_4)=a_object_set_3;
    f_sel_4_FIELD (F_FIELD a_lab_rang_1 a_lab_rang_2 a_object_set_3 a_lnlbra_4)=a_lnlbra_4;
data 
    T_labrow=F_LABROW [T_lab_range] T_object_set;
    f_sel_1_LABROW (F_LABROW a_lab_rang_1 a_object_set_2)=a_lab_rang_1;
    f_sel_2_LABROW (F_LABROW a_lab_rang_1 a_object_set_2)=a_object_set_2;
data 
    T_lnlbran=F_LNLBRAN Int [T_lab_range];
    f_sel_1_LNLBRAN (F_LNLBRAN a_int_1 a_lab_rang_2)=a_int_1;
    f_sel_2_LNLBRAN (F_LNLBRAN a_int_1 a_lab_rang_2)=a_lab_rang_2;
    f_sel_lenb (F_LNLBRAN a_n a_r)=a_n;
    f_ap_label::T_object_set -> T_lab_range -> T_lab_range;
    f_ap_label a_sset (F_LAB_RANGE a_lab a_pix)=
        let { 
            r_s=f_find a_sset a_lab
         } in  
            if (f_set_eq f_cmp_label r_s c_no_object)
            then (F_LAB_RANGE c_no_label a_pix)
            else 
                (F_LAB_RANGE (f_top r_s) a_pix);
    f_field2pix::Int -> Int -> [T_lab_range] -> [T_label];
    f_field2pix a_size a_st []=f_map (f_const c_no_label) (f_xcount a_st a_size);
    f_field2pix a_size a_st ((F_LAB_RANGE a_lab (F_RANGE a_bl a_wh)):a_rest)=
        let { 
            r_whites=f_map (f_const c_no_label) (f_xcount a_st a_bl);
            r_blacks=f_map (f_const a_lab) (f_xcount a_bl a_wh)
         } in  (++) r_whites ((++) r_blacks (f_field2pix a_size a_wh a_rest));
    f_show_lnlbran::T_lnlbran -> [Char];
    f_show_lnlbran (F_LNLBRAN a_l a_r)=(:) '{' ((++) (strict_show_i a_l) ((++) " | " ((++) (f_concat 
        (f_map ((.) ((:) ' ') f_show_lab_range) a_r)) "}")));
    f_show_lab_range::T_lab_range -> [Char];
    f_show_lab_range (F_LAB_RANGE a_lb a_ran)=(++) (strict_show_i a_lb) (f_show_range a_ran);
    f_c2pix::Char -> T_pixel;
    f_c2pix a_c=
        if (((==) :: (Int -> Int -> Bool)) (fromEnum a_c) (fromEnum 'B'))
        then C_Black
        else 
            C_White;
    c_figure_l_l=f_map (f_map f_c2pix) ((:) "      BBBBBBBBBB      BBBBBBBBBB" ((:) " BBB  BB      BB BBB  BB      BB" ((:) "B   B BBB   B BBB   B BBB   B BB" ((:) "B  BBB BB  BBB BB  BBB BB  BBB B" 
        ((:) "BB  BB BBB  BB BBB  BB BBB  BB B" ((:) "BBB    BBBB    BBBB    BBBB    B" ((:) " BBB  BB BBB  BB BBB  BB BBB  BB" ((:) "  BB BBB  BB BBB  BB BBB  BB BBB" ((:) "B  BBB B BBB  BBB  BBB B BBB  BB" ((:) "BB  BB BBB  BB BBB  BB BBB  BB B" ((:) "BBB    BBB  BB BBBB    BBB  BB B" 
        ((:) "BBB    BBB  BB BBBB    BBB  BB B" ((:) " BBB  BBBBB    B BBB  BBBBB    B" ((:) "B  BBB BBBB    BB  BBB BBBB    B" ((:) "  BB BBB BBB  BB  BB BBB BBB  BB" ((:) " BBB  BB      BB BBB  BB      BB" ((:) "      BBBBBBBBBB      BBBBBBBBBB" ((:) " BBB  BB      BB BBB  BB      BB" 
        ((:) "B   B BBB   B BBB   B BBB   B BB" ((:) "B  BBB BB  BBB BB  BBB BB  BBB B" ((:) "BB  BB BBB  BB BBB  BB BBB  BB B" ((:) "BBB    BBBB    BBBB    BBBB    B" ((:) " BBB  BB BBB  BB BBB  BB BBB  BB" ((:) "  BB BBB  BB BBB  BB BBB  BB BBB" ((:) "B  BBB B BBB  BBB  BBB B BBB  BB" 
        ((:) "BB  BB BBB  BB BBB  BB BBB  BB B" ((:) "BBB    BBB  BB BBBB    BBB  BB B" ((:) "BBB    BBB  BB BBBB    BBB  BB B" ((:) " BBB  BBBBB    B BBB  BBBBB    B" ((:) "B  BBB BBBB    BB  BBB BBBB    B" ((:) "  BB BBB BBB  BB  BB BBB BBB  BB" ((:) " BBB  BB      BB BBB  BB      BB" []))))))))))))))))))))))))))))))));
    c_figure_l=f_map (f_map f_c2pix) ((:) "      BBBBBBBBBB" ((:) " BBB  BB      BB" ((:) "B   B BBB   B BB" ((:) "B  BBB BB  BBB B" 
        ((:) "BB  BB BBB  BB B" ((:) "BBB    BBBB    B" ((:) " BBB  BB BBB  BB" ((:) "  BB BBB  BB BBB" ((:) "B  BBB B BBB  BB" ((:) "BB  BB BBB  BB B" ((:) "BBB    BBB  BB B" 
        ((:) "BBB    BBB  BB B" ((:) " BBB  BBBBB    B" ((:) "B  BBB BBBB    B" ((:) "  BB BBB BBB  BB" ((:) " BBB  BB      BB" []))))))))))))))));
    c_figure=f_map (f_map f_c2pix) ((:) "        " ((:) "B   B BB" ((:) "B  BBB B" ((:) "BB  BB B" 
        ((:) "BBB    B" ((:) " BBB  BB" ((:) "  BB BBB" ((:) "      BB" []))))))));
    c_small_figure=f_map (f_map f_c2pix) ((:) "B " ((:) "BB" []));
    c_mini_figure=f_map (f_map f_c2pix) ((:) "B" []);
    f_n_app::Int -> [t1] -> [t1];
    f_n_app 0 a_lst=[];
    f_n_app a_n a_lst=(++) a_lst (f_n_app (((-) :: (Int -> Int -> Int)) a_n (1 :: Int)) a_lst);
    f_n_fig::Int -> [[t1]] -> [[t1]];
    f_n_fig a_n a_fig=f_n_app a_n (f_map (f_n_app a_n) a_fig);
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
