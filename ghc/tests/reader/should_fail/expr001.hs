module ShouldFail where

{-
From: Kevin Hammond <kh>
To: partain
Subject: Re:  parsing problem w/ queens
Date: Wed, 9 Oct 91 17:31:46 BST

OK, I've fixed that little problem by disallowing,
-}
module Test where

f x = x + if True then 1 else 2
f x = x + 1::Int

-- (the conditional/sig need to be parenthesised).  If this is
-- problematic, let me know!
