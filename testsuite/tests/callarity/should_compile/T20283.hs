module T20283 where

import Data.Array

polynomial ::  (Array (Int, Int) (Array (Int, Int) Double)) -> Int -> (Array Int Double) -> (Array Int Double) -> Double -> Double -> Double
polynomial g degree rho_table theta_table rho_value theta_value =

   let {
       table_search table value =
          let {
              (low, high) = bounds table ;
              search_down i = if value > (table!(i - 1)) then
                                i else search_down (i - 1)
              } in if value > (table!high) then
                     high + 1 else if value <= (table!low) then
                                     low else search_down high ;
       rho_index = table_search rho_table rho_value ;
       theta_index = table_search theta_table theta_value ;
       a = g!(rho_index, theta_index)
       } in foldl (+) 0 [ ((a!(i, j)) * rho_value ^ (i::Int))
                          * theta_value ^ j | i <- [0..degree],
                                              j <- [0..degree] ]

