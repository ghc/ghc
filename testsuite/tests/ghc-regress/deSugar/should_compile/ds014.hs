-- !!! ds014 -- character and string literals
-- !!!   really should add ALL weird forms...

module ShouldCompile where

a = 'a'
b = "b"
c = a:b
d = b ++ b

b1 = ""		-- examples from the Haskell report
b2 = "\&"	-- the same thing
b3 = "\SO\&H" ++ "\137\&9"

a000 = '\NUL'
a001 = '\SOH'
a002 = '\STX'
a003 = '\ETX'
a004 = '\EOT'
a005 = '\ENQ'
a006 = '\ACK'
a007 = '\BEL'
a010 = '\BS'
a011 = '\HT'
a012 = '\LF'
a013 = '\VT'
a014 = '\FF'
a015 = '\CR'
a016 = '\SO'
a017 = '\SI'
a020 = '\DLE'
a021 = '\DC1'
a022 = '\DC2'
a023 = '\DC3'
a024 = '\DC4'
a025 = '\NAK'
a026 = '\SYN'
a027 = '\ETB'
a030 = '\CAN'
a031 = '\EM'
a032 = '\SUB'
a033 = '\ESC'
a034 = '\FS'
a035 = '\GS'
a036 = '\RS'
a037 = '\US'
a040 = '\SP'
a042 = '"'
a047 = '\''
a134 = '\\'
a177 = '\DEL'

ascii = "\NUL\SOH\STX\ETX\EOT\ENQ\ACK\BEL\
    	\\BS\HT\LF\VT\FF\CR\SO\SI\
	\\DLE\DC1\DC2\DC3\DC4\NAK\SYN\ETB\
	\\CAN\EM\SUB\ESC\FS\GS\RS\US\
	\\SP!\"#$%&'\
	\()*+,-./\
	\01234567\
	\89:;<=>?\
	\@ABCDEFG\
	\HIJKLMNO\
	\PQRSTUVW\
	\XYZ[\\]^_\
	\`abcdefg\
	\hijklmno\
	\pqrstuvw\
	\xyz{|}~\DEL"

na200 = '\o200'
na250 = '\o250'
na300 = '\o300'
na350 = '\o350'
na377 = '\o377'

eightbit = "\o200\o250\o300\o350\o377"
