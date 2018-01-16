import Text.Printf

main = do
   printf "%.*f\n" (2::Int) ((1/3) :: Double)
   -- (expected: "0.33")

   printf "%.3s\n" "foobar"
   -- (expected: "foo")

   printf "%10.5d\n" (4::Int)
   -- (expected: "     00004")
