/*
   Implementation of optimally compiled list comprehensions using Wadler's algorithm from
   Peyton-Jones "Implementation of Functional Programming Languages", 1987

   TQ transforms a list of qualifiers (either boolean expressions or generators) into a
   single expression which implements the list comprehension.

   TE << [E || Q] >>  =  TQ <<  [E || Q] ++ [] >>

   TQ << [E || p <- L1, Q]  ++  L2 >> =

      h ( TE << L1 >> ) where
      			h = us -> case us in
	  			[]        ->  TE << L2 >>
				(u : us') ->
					(TE << p >> ->  ( TQ << [E || Q]  ++  (h us') >> )) u
 */

tree TQ(quals,l2)
list quals, l2;
{
  tree qualh;
  list rest;

  if(tlist(quals) == lnil)
    return(mkcons(zfexpr,l2));

  qualh = (tree) lhd(quals);
  rest = ltl(quals);

  if(ttree(qualh) != qual)
    return(mkif(qualh,TQ(rest,l2),l2));

  {
    tree h = mkident(uniqueident("Zh%d")),
         u = mkident(uniqueident("Iu%d")),
         us = mkident(uniqueident("Ius%d")),
         pat = gqpat(qualh);

    pbinding tq = mkppat(gqpat(qualh),TQ(rest,mkap(h,us)));


    return(
       mkletv(
	 mkrbind(
	   mkpbind(
	     lsing(
	       mkppat(h,
		 mklam(us,
		   mkcasee(us,
		     ldub(
		       mkppat(niltree,l2),
		       mkppat(
			  mkcons(u,us),
			  mkcasee(u,lsing(tq))
/*
  replaces the following code which elides patterns in list comprehensions a la M*****a

			  mkcasee(u,
			    ttree(pat) == ident && !isconstr(gident(pat))?
			      lsing(tq):
			      ldub(tq,mkppat(mkident("_"),mkap(h,us))))
*/
			    )))))))),
	 mkap(h,gqexp(qualh))));
  }
}
