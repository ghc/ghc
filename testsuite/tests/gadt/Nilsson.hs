{-# LANGUAGE GADTs, ScopedTypeVariables #-}

-- Supplied by Henrik Nilsson, showed up a bug in GADTs 

module Nilsson where

data Event a = NoEvent | Event a

fromEvent :: Event a -> a
fromEvent = undefined

usrErr :: String -> String -> String -> a
usrErr = undefined

type DTime = Double	-- [s]

data SF a b = SF {sfTF :: a -> Transition a b}

data SF' a b where
    SFArr   :: (DTime -> a -> Transition a b) -> FunDesc a b -> SF' a b

    SFAcc   :: (DTime -> Event a -> Transition (Event a) b)
               -> (c -> a -> (c, b)) -> c -> b
               -> SF' (Event a) b
    SFCpAXA :: (DTime -> a -> Transition a d)
               -> FunDesc a b -> SF' b c -> FunDesc c d
               -> SF' a d
    SF' :: (DTime -> a -> Transition a b) -> SF' a b

-- A transition is a pair of the next state (in the form of a signal
-- function) and the output at the present time step.

type Transition a b = (SF' a b, b)


sfTF' :: SF' a b -> (DTime -> a -> Transition a b)
sfTF' (SFArr   tf _)     = tf
sfTF' (SFAcc   tf _ _ _) = tf
-- sfTF' (SFSScan ...)
sfTF' (SFCpAXA tf _ _ _) = tf
sfTF' (SF' tf)           = tf

-- "Smart" constructors. The corresponding "raw" constructors should not
-- be used directly for construction.

sfArr :: FunDesc a b -> SF' a b
sfArr FDI         = sfId
sfArr (FDC b)     = sfConst b
sfArr (FDE f fne) = sfArrE f fne
sfArr (FDG f)     = sfArrG f


sfId :: SF' a a
sfId = sf
    where
	sf = SFArr (\_ a -> (sf, a)) FDI


sfConst :: b -> SF' a b
sfConst b = sf
    where
	sf = SFArr (\_ _ -> (sf, b)) (FDC b)


sfNever :: SF' a (Event b)
sfNever = sfConst NoEvent


-- Assumption: fne = f NoEvent
sfArrE :: (Event a -> b) -> b -> SF' (Event a) b
sfArrE f fne = sf
    where
        sf  = SFArr (\_ ea -> (sf, case ea of NoEvent -> fne ; _ -> f ea))
                    (FDE f fne)

sfArrG :: (a -> b) -> SF' a b
sfArrG f = sf
    where
	sf = SFArr (\_ a -> (sf, f a)) (FDG f)


sfAcc :: (c -> a -> (c, b)) -> c -> b -> SF' (Event a) b
sfAcc f c bne = sf
    where
        sf = SFAcc (\dt ea -> case ea of
                                  NoEvent -> (sf, bne)
                                  Event a -> let
                                                 (c', b) = f c a
                                             in
                                                 (sfAcc f c' bne, b))
                   f
                   c
                   bne

-- sfAccHld would be very similar. The only difference is that
-- what's now called "bne" would be updated at each event.
--
-- So maybe one could use the SAME constructor, just different
-- transition functions? It really depends on what assumptions
-- one need to make when optimizing.


-- Motivation for event-processing function type
-- (alternative would be function of type a->b plus ensuring that it
-- only ever gets invoked on events):
-- * Now we need to be consistent with other kinds of arrows.
-- * We still want to be able to get hold of the original function.

data FunDesc a b where
    FDI :: FunDesc a a					-- Identity function
    FDC :: b -> FunDesc a b				-- Constant function
    FDE :: (Event a -> b) -> b -> FunDesc (Event a) b	-- Event-processing fun
    FDG :: (a -> b) -> FunDesc a b			-- General function

fdFun :: FunDesc a b -> (a -> b)
fdFun FDI       = id
fdFun (FDC b)   = const b
fdFun (FDE f _) = f
fdFun (FDG f)   = f

fdComp :: FunDesc a b -> FunDesc b c -> FunDesc a c
fdComp FDI           fd2     = fd2
fdComp fd1           FDI     = fd1
fdComp (FDC b)       fd2     = FDC ((fdFun fd2) b)
fdComp _             (FDC c) = FDC c
fdComp (FDE f1 f1ne) fd2 = FDE (f2 . f1) (f2 f1ne)
    where
        f2 = fdFun fd2
fdComp (FDG f1) (FDE f2 f2ne) = FDG f
    where
        f a = case f1 a of
                  NoEvent -> f2ne
                  f1a     -> f2 f1a
fdComp (FDG f1) fd2 = FDG (fdFun fd2 . f1)


-- Verifies that the first argument is NoEvent. Returns the value of the
-- second argument that is the case. Raises an error otherwise.
-- Used to check that functions on events do not map NoEvent to Event
-- wherever that assumption is exploited.
vfyNoEv :: Event a -> b -> b
vfyNoEv NoEvent b = b
vfyNoEv _       _  = usrErr "AFRP" "vfyNoEv"
                            "Assertion failed: Functions on events must not \
			    \map NoEvent to Event."

compPrim :: SF a b -> SF b c -> SF a c
compPrim (SF {sfTF = tf10}) (SF {sfTF = tf20}) = SF {sfTF = tf0}
    where
	tf0 a0 = (cpXX sf1 sf2, c0)
	    where
		(sf1, b0) = tf10 a0
		(sf2, c0) = tf20 b0

	-- Naming convention: cp<X><Y> where  <X> and <Y> is one of:
        -- X - arbitrary signal function
        -- A - arbitrary pure arrow
        -- C - constant arrow
        -- E - event-processing arrow
        -- G - arrow known not to be identity, constant (C) or
        --     event-processing (E).

        cpXX :: SF' a b -> SF' b c -> SF' a c
        cpXX (SFArr _ fd1)       sf2               = cpAX fd1 sf2
        cpXX sf1                 (SFArr _ fd2)     = cpXA sf1 fd2
        cpXX (SFAcc _ f1 s1 bne) (SFAcc _ f2 s2 cne) =
            sfAcc f (s1, s2) (vfyNoEv bne cne)
	    where
		f (s1, s2) a =
		    case f1 s1 a of
			(s1', NoEvent) -> ((s1', s2), cne)
			(s1', Event b) ->
			    let (s2', c) = f2 s2 b in ((s1', s2'), c)
        cpXX (SFCpAXA _ fd11 sf12 fd13) (SFCpAXA _ fd21 sf22 fd23) =
            cpAXA fd11 (cpXX (cpXA sf12 (fdComp fd13 fd21)) sf22) fd23
	cpXX sf1 sf2 = SF' tf
	    where
	        tf dt a = (cpXX sf1' sf2', c)
		    where
		        (sf1', b) = (sfTF' sf1) dt a
			(sf2', c) = (sfTF' sf2) dt b

        cpAXA :: FunDesc a b -> SF' b c -> FunDesc c d -> SF' a d
        cpAXA FDI     sf2 fd3     = cpXA sf2 fd3
        cpAXA fd1     sf2 FDI     = cpAX fd1 sf2
        cpAXA (FDC b) sf2 fd3     = cpCXA b sf2 fd3
        cpAXA fd1     sf2 (FDC d) = sfConst d        
        cpAXA fd1 (SFArr _ fd2) fd3 = sfArr (fdComp (fdComp fd1 fd2) fd3)

	cpAX :: FunDesc a b -> SF' b c -> SF' a c
        cpAX FDI           sf2 = sf2
        cpAX (FDC b)       sf2 = cpCX b sf2
        cpAX (FDE f1 f1ne) sf2 = cpEX f1 f1ne sf2
        cpAX (FDG f1)      sf2 = cpGX f1 sf2

	cpXA :: SF' a b -> FunDesc b c -> SF' a c
        cpXA sf1 FDI           = sf1
        cpXA sf1 (FDC c)       = sfConst c
        cpXA sf1 (FDE f2 f2ne) = cpXE sf1 f2 f2ne
        cpXA sf1 (FDG f2)      = cpXG sf1 f2

        cpCX :: b -> SF' b c -> SF' a c
        cpCX b (SFArr _ fd2)              = sfConst ((fdFun fd2) b)
        cpCX b (SFAcc _ _ _ cne)          = sfConst (vfyNoEv b cne)
        cpCX b (SFCpAXA _ fd21 sf22 fd23) =
            cpCXA ((fdFun fd21) b) sf22 fd23
	cpCX b sf2 = SFCpAXA tf (FDC b) sf2 FDI
	    where
		tf dt _ = (cpCX b sf2', c)
		    where
			(sf2', c) = (sfTF' sf2) dt b

-- For SPJ: The following version did not work. 
-- The commented out one below did work, by lambda-lifting cpCXAux
        cpCXA :: b -> SF' b c -> FunDesc c d -> SF' a d
        cpCXA b sf2 FDI     = cpCX b sf2
        cpCXA _ _   (FDC c) = sfConst c
        cpCXA b (sf2 :: SF' b c) (fd3 :: FunDesc c d) = cpCXAAux sf2
            where
                f3  = fdFun fd3

		cpCXAAux :: SF' b c -> SF' a d
                cpCXAAux (SFArr _ fd2) = sfConst (f3 ((fdFun fd2) b))
                cpCXAAux (SFAcc _ _ _ cne) = sfConst (vfyNoEv b (f3 cne))
                cpCXAAux (SFCpAXA _ fd21 sf22 fd23) = cpCXA ((fdFun fd21) b) sf22 (fdComp fd23 fd3)

{- -- For SPJ: This version works
        cpCXA :: b -> SF' b c -> FunDesc c d -> SF' a d
        cpCXA b sf2 FDI     = cpCX b sf2
        cpCXA _ _   (FDC c) = sfConst c
        cpCXA b sf2 fd3 = cpCXAAux b fd3 (fdFun fd3) sf2
            where
                -- f3  = fdFun fd3
		-- Really something like: cpCXAAux :: SF' b c -> SF' a d
                cpCXAAux :: b -> FunDesc c d -> (c -> d) -> SF' b c -> SF' a d
                cpCXAAux b fd3 f3 (SFArr _ fd2) = sfConst (f3 ((fdFun fd2) b))
                cpCXAAux b fd3 f3 (SFAcc _ _ _ cne) = sfConst (vfyNoEv b (f3 cne))
                cpCXAAux b fd3 f3 (SFCpAXA _ fd21 sf22 fd23) = cpCXA ((fdFun fd21) b) sf22 (fdComp fd23 fd3)
-}

        cpGX :: (a -> b) -> SF' b c -> SF' a c
	cpGX f1 (SFArr _ fd2) = sfArr (fdComp (FDG f1) fd2)
        cpGX f1 (SFCpAXA _ fd21 sf22 fd23) =
            cpAXA (fdComp (FDG f1) fd21) sf22 fd23
	cpGX f1 sf2 = SFCpAXA tf (FDG f1) sf2 FDI
	    where
		tf dt a = (cpGX f1 sf2', c)
		    where
			(sf2', c) = (sfTF' sf2) dt (f1 a)

        cpXG :: SF' a b -> (b -> c) -> SF' a c
	cpXG (SFArr _ fd1) f2 = sfArr (fdComp fd1 (FDG f2))
        cpXG (SFAcc _ f1 s bne) f2 = sfAcc f s (f2 bne)
            where
                f s a = let (s', b) = f1 s a in (s', f2 b)
	cpXG (SFCpAXA _ fd11 sf12 fd22) f2 =
            cpAXA fd11 sf12 (fdComp fd22 (FDG f2))
	cpXG sf1 f2 = SFCpAXA tf FDI sf1 (FDG f2)
	    where
		tf dt a = (cpXG sf1' f2, f2 b)
		    where
			(sf1', b) = (sfTF' sf1) dt a

        cpEX :: (Event a -> b) -> b -> SF' b c -> SF' (Event a) c
	cpEX f1 f1ne (SFArr _ fd2) = sfArr (fdComp (FDE f1 f1ne) fd2)
	cpEX f1 f1ne (SFAcc _ f2 s cne) = sfAcc f s (vfyNoEv f1ne cne)
            where
                f s a = f2 s (fromEvent (f1 (Event a)))
	cpEX f1 f1ne (SFCpAXA _ fd21 sf22 fd23) =
            cpAXA (fdComp (FDE f1 f1ne) fd21) sf22 fd23
	cpEX f1 f1ne sf2 = SFCpAXA tf (FDE f1 f1ne) sf2 FDI
	    where
		tf dt ea = (cpEX f1 f1ne sf2', c)
		    where
                        (sf2', c) = case ea of
				        NoEvent -> (sfTF' sf2) dt f1ne
					_       -> (sfTF' sf2) dt (f1 ea)

	cpXE :: SF' a (Event b) -> (Event b -> c) -> c -> SF' a c
        cpXE (SFArr _ fd1)   f2 f2ne = sfArr (fdComp fd1 (FDE f2 f2ne))
        cpXE (SFAcc _ f1 s bne) f2 f2ne = sfAcc f s (vfyNoEv bne f2ne)
            where
                f s a = let (s', eb) = f1 s a
                        in
                            case eb of NoEvent -> (s', f2ne); _ -> (s', f2 eb)
        cpXE (SFCpAXA _ fd11 sf12 fd13) f2 f2ne =
            cpAXA fd11 sf12 (fdComp fd13 (FDE f2 f2ne))
	cpXE sf1 f2 f2ne = SFCpAXA tf FDI sf1 (FDE f2 f2ne)
	    where
		tf dt a = (cpXE sf1' f2 f2ne,
                           case eb of NoEvent -> f2ne; _ -> f2 eb)
		    where
                        (sf1', eb) = (sfTF' sf1) dt a
