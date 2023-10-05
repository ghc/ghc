module Main (main) -- q
where {
--import Fast2haskell;

    f_queens a_n=f_queens' (enumFromTo (1 :: Int) a_n) a_n;
    f_queens' a_positions 0=(:) [] [];
    f_queens' a_positions a_n=c_concat (f_map (f_place (f_queens' a_positions (((-) :: (Int -> Int -> Int)) a_n (1 :: Int)))) a_positions);
    f_place a_boards a_q=[(:) a_q a_bs|a_bs<-a_boards,f_safe (1 :: Int) a_q a_bs];
    f_safe a_d a_q []=True;
    f_safe a_d a_q (a_h:a_t)=
        if (((==) :: (Int -> Int -> Bool)) a_q a_h)
        then False
        else 
        if (((==) :: (Int -> Int -> Bool)) (f_absi (((-) :: (Int -> Int -> Int)) a_q a_h)) a_d)
        then False
        else 
            (f_safe (((+) :: (Int -> Int -> Int)) a_d (1 :: Int)) a_q a_t);
    f_absi a_n=
        if (((<) :: (Int -> Int -> Bool)) a_n (0 :: Int))
        then (((negate) :: (Int -> Int)) a_n)
        else 
            a_n;
    f_main a_n=(++) (show (length (f_queens a_n))) "\n";
    c_input=(10 :: Int);
    c_concat=f_foldr (++) [];
    f_foldr a_op a_r []=a_r;
    f_foldr a_op a_r (a_a:a_x)=a_op a_a (f_foldr a_op a_r a_x);
    f_map a_f a_x=[a_f a_a|a_a<-a_x];
    main = putStr (f_main c_input)
}
