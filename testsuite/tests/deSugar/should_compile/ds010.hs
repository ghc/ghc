-- !!! ds010 -- deeply-nested list comprehensions

module ShouldCompile where

z = [ (a,b,c,d,e,f,g,h,i,j) | a <- "12",
			      b <- "12",
			      c <- "12",
			      d <- "12",
			      e <- "12",
			      f <- "12",
			      g <- "12",
			      h <- "12",
			      i <- "12",
			      j <- "12"
    ]
