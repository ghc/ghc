1. I want to eliminate the duplicate of this library in
GHC. The underlying TextDetails store types are different
though which is a problem:

 -> Use type classes (see new-pretty branch).
    [Duncan not a fan.]
	 [Make sure performance not lost.]

 -> Use a better underlying storage type.
    [Say add bytestring and text to TextDetails.]

 -> Use a fixed underlying storage type like
    Builder from Text or Bytestring but allow
	 input functions to take any type that can be
	 converted into one.
	 [ptext :: (a -> Builder) -> a -> Doc

Also would be useful to provide render functions to produce
Bytestring / Text builders.

2. Add monadic wrapper (check out haskell-src-exts for example although maybe
use a transformer monand...)

===========================================================

dcoutts		davidt_: are you sure that using a typeclass is faster than TextDetails?

dcoutts		davidt_: why not just add PStr back in?

dcoutts		davidt_: you can already generate different output types by using the fold (TextDetails -> a -> a)

dcoutts	e.g. using the Text builder or the new bytestring builder

davidt_	dcoutts: So far it seems as fast but I need to do more testing, hence I haven't pushed anything yet

davidt_	dcoutts: Yes adding PStr back in is one option but I also need to change LItString in GHC then to be backed by a bytestring which is a decent amount of work on a area thats already very boring

davidt_	dcoutts: as long as speed isn't lost I still feel a type class is better, you can generate different outputs yes but a fixed TextDetails still fixes the storage which isn't as nice as a type class imho

dcoutts		davidt_: the problem with the typeclass is the leakage

dcoutts	that extra type param leaks out into everything

dcoutts		davidt_: and it doesn't mean you have to change LItString to be a ByteString

dcoutts		davidt_: it just means you need a conversion function, it doesn't imply any copying either since it's lazy, it'll do the conversion during the fullRender

davidt_	yes i guess so, there are a few options here. What is the issue with the leakage though? It sounds bad but how is it practically a bad thing? I quite like the type class design

dcoutts	I think we overuse typeclasses 

dcoutts		davidt_: it means your pretty printing function producing a Doc will not be compatible with mine

dcoutts		davidt_: since you'll use GDoc This and I'll use GDoc That...

dcoutts	and in this case it is for variation that isn't really needed
dcoutts	it's to cope with the proliferation of string types
dcoutts	when we should just not have so many string types
dcoutts		davidt_: so how about using TextDetails with constructors for Char, String, Text and ByteString

davidt_	Hmm I'll look into it I guess.

davidt_	But I think what I want to do is a pretty simple and 'good' thing to do. I want to abstract pretty from the underlying storage of strings. As far as I can tell type classes is the best way to do this.

davidt_	but I agree that we have too many string types

davidt_	so I am tempted by that argument not to encourage it further

dcoutts		davidt_: btw, I expect you can convert a ghc LItString into a ByteString quite easily and cheaply

dcoutts		davidt_: or are they unpinned ByteArr#s?

davidt_	dcoutts: Yes you probably can. Had a brief discussion about this with Simon Marlow.

dcoutts		davidt_: so there's a couple other options here

dcoutts		davidt_: you can fix the output type and allow any input string type that can be converted into it

dcoutts		davidt_: or you can fix the set of primitive input string types (ie Char, String, etc) and allow any kind of output type that can be constructed from those

dcoutts		davidt_: but allowing both means that the internal type arg has to become visible (which is the bad option imho)

dcoutts		davidt_: e.g. suppose we said that the output type should just always be a Text builder, or perhaps a ByteString builder, then we could allow primitive strings of any type that can be converted to a bytestring builder

dcoutts	ptext :: (a -> Builder) -> a -> doc

dcoutts		davidt_: in practice I bet fullRender is only used for two types: IO to write out to a handle directly, and some builder monoid

dcoutts	and the IO case is only an illusion of performance, the builder monoid will be a lot faster

dcoutts		davidt_: because a builder monoid is writing directly into a buffer too, but unlike an IO handle, there's no MVar locking overhead

dcoutts		davidt_: whichever way you do go, it'd be nice to provide render functions to produce bytestring / text builders, since people will generally not be aware that that's possible via fullRender

dcoutts		davidt_: the next bytestring release will have a fast builder monoid

dcoutts		davidt_: and text has one already

