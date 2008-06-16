{-# LANGUAGE Arrows #-}

-- test for out-size tuples: takes a _long_ time to compile

module ShouldCompile where

import Control.Arrow

data T1 = C1
data T2 = C2
data T3 = C3
data T4 = C4
data T5 = C5
data T6 = C6
data T7 = C7
data T8 = C8
data T9 = C9
data T10 = C10
data T11 = C11
data T12 = C12
data T13 = C13
data T14 = C14
data T15 = C15
data T16 = C16
data T17 = C17
data T18 = C18
data T19 = C19
data T20 = C20
data T21 = C21
data T22 = C22
data T23 = C23
data T24 = C24
data T25 = C25
data T26 = C26
data T27 = C27
data T28 = C28
data T29 = C29
data T30 = C30
data T31 = C31
data T32 = C32
data T33 = C33
data T34 = C34
data T35 = C35
data T36 = C36
data T37 = C37
data T38 = C38
data T39 = C39
data T40 = C40
data T41 = C41
data T42 = C42
data T43 = C43
data T44 = C44
data T45 = C45
data T46 = C46
data T47 = C47
data T48 = C48
data T49 = C49
data T50 = C50
data T51 = C51
data T52 = C52
data T53 = C53
data T54 = C54
data T55 = C55
data T56 = C56
data T57 = C57
data T58 = C58
data T59 = C59
data T60 = C60
data T61 = C61
data T62 = C62
data T63 = C63
data T64 = C64
data T65 = C65
data T66 = C66
data T67 = C67
data T68 = C68
data T69 = C69
data T70 = C70

f :: Arrow a => a Int Int
f = proc x0 -> do
	x1 <- returnA -< C1
	x2 <- returnA -< C2
	x3 <- returnA -< C3
	x4 <- returnA -< C4
	x5 <- returnA -< C5
	x6 <- returnA -< C6
	x7 <- returnA -< C7
	x8 <- returnA -< C8
	x9 <- returnA -< C9
	x10 <- returnA -< C10
	x11 <- returnA -< C11
	x12 <- returnA -< C12
	x13 <- returnA -< C13
	x14 <- returnA -< C14
	x15 <- returnA -< C15
	x16 <- returnA -< C16
	x17 <- returnA -< C17
	x18 <- returnA -< C18
	x19 <- returnA -< C19
	x20 <- returnA -< C20
	x21 <- returnA -< C21
	x22 <- returnA -< C22
	x23 <- returnA -< C23
	x24 <- returnA -< C24
	x25 <- returnA -< C25
	x26 <- returnA -< C26
	x27 <- returnA -< C27
	x28 <- returnA -< C28
	x29 <- returnA -< C29
	x30 <- returnA -< C30
	x31 <- returnA -< C31
	x32 <- returnA -< C32
	x33 <- returnA -< C33
	x34 <- returnA -< C34
	x35 <- returnA -< C35
	x36 <- returnA -< C36
	x37 <- returnA -< C37
	x38 <- returnA -< C38
	x39 <- returnA -< C39
	x40 <- returnA -< C40
	x41 <- returnA -< C41
	x42 <- returnA -< C42
	x43 <- returnA -< C43
	x44 <- returnA -< C44
	x45 <- returnA -< C45
	x46 <- returnA -< C46
	x47 <- returnA -< C47
	x48 <- returnA -< C48
	x49 <- returnA -< C49
	x50 <- returnA -< C50
	x51 <- returnA -< C51
	x52 <- returnA -< C52
	x53 <- returnA -< C53
	x54 <- returnA -< C54
	x55 <- returnA -< C55
	x56 <- returnA -< C56
	x57 <- returnA -< C57
	x58 <- returnA -< C58
	x59 <- returnA -< C59
	x60 <- returnA -< C60
	x61 <- returnA -< C61
	x62 <- returnA -< C62
	x63 <- returnA -< C63
	x64 <- returnA -< C64
	x65 <- returnA -< C65
	x66 <- returnA -< C66
	x67 <- returnA -< C67
	x68 <- returnA -< C68
	x69 <- returnA -< C69
	x70 <- returnA -< C70
	returnA -< x70
	returnA -< x69
	returnA -< x68
	returnA -< x67
	returnA -< x66
	returnA -< x65
	returnA -< x64
	returnA -< x63
	returnA -< x62
	returnA -< x61
	returnA -< x60
	returnA -< x59
	returnA -< x58
	returnA -< x57
	returnA -< x56
	returnA -< x55
	returnA -< x54
	returnA -< x53
	returnA -< x52
	returnA -< x51
	returnA -< x50
	returnA -< x49
	returnA -< x48
	returnA -< x47
	returnA -< x46
	returnA -< x45
	returnA -< x44
	returnA -< x43
	returnA -< x42
	returnA -< x41
	returnA -< x40
	returnA -< x39
	returnA -< x38
	returnA -< x37
	returnA -< x36
	returnA -< x35
	returnA -< x34
	returnA -< x33
	returnA -< x32
	returnA -< x31
	returnA -< x30
	returnA -< x29
	returnA -< x28
	returnA -< x27
	returnA -< x26
	returnA -< x25
	returnA -< x24
	returnA -< x23
	returnA -< x22
	returnA -< x21
	returnA -< x20
	returnA -< x19
	returnA -< x18
	returnA -< x17
	returnA -< x16
	returnA -< x15
	returnA -< x14
	returnA -< x13
	returnA -< x12
	returnA -< x11
	returnA -< x10
	returnA -< x9
	returnA -< x8
	returnA -< x7
	returnA -< x6
	returnA -< x5
	returnA -< x4
	returnA -< x3
	returnA -< x2
	returnA -< x1
	returnA -< x0
