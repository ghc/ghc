module ShouldCompile where

postInlineUnconditionally 
  = case Just "Hey" of
	-- The point of examining occ_info here is that for *non-values* 
	-- that occur outside a lambda, the call-site inliner won't have
	-- a chance (because it doesn't know that the thing
	-- only occurs once).   The pre-inliner won't have gotten
	-- it either, if the thing occurs in more than one branch
	-- So the main target is things like
	--	let x = f y in
	--	case v of
	--	   True  -> case x of ...
	--	   False -> case x of ...
	-- I'm not sure how important this is in practice
      Just a	-- OneOcc => no work-duplication issue
	-> True	-- Small enough to dup
			-- ToDo: consider discount on smallEnoughToInline if int_cxt is true
			--
		 	-- NB: Do NOT inline arbitrarily big things, even if one_br is True
			-- Reason: doing so risks exponential behaviour.  We simplify a big
			--	   expression, inline it, and simplify it again.  But if the
			--	   very same thing happens in the big expression, we get 
			--	   exponential cost!
			-- PRINCIPLE: when we've already simplified an expression once, 
			-- make sure that we only inline it if it's reasonably small.

      _ -> False

-- Here's an example that we don't handle well:
--	let f = if b then Left (\x.BIG) else Right (\y.BIG)
--	in \y. ....case f of {...} ....
-- Here f is used just once, and duplicating the case work is fine (exprIsCheap).
-- But
-- * We can't preInlineUnconditionally because that woud invalidate
--   the occ info for b.  
-- * We can't postInlineUnconditionally because the RHS is big, and
--   that risks exponential behaviour
-- * We can't call-site inline, because the rhs is big
-- Alas!

  where
    x = id

