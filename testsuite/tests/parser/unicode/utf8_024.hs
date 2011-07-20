{-# LANGUAGE ExplicitForAll, UnicodeSyntax #-}
{-
Test for valid unicode identifiers

*** This file is UTF-8 encoded.

*** BE CAREFUL WHEN EDITING THIS FILE WITH EMACS.  Emacs' UTF-8 engine
    has several times got the encoding wrong for me and inserted bogus
    bytes, especially in the 4-byte characters.  Edit the file literally
    (M-x find-file-literally).  By all means view it in Emacs' UTF-8
    mode (C-x RET c utf-8, C-x f unicode001.hs), but don't edit and save.

Here's a selection of characters I pulled from UnicodeData.txt that we
can use to test with:

-- upper/lower case letters
À	LATIN CAPITAL LETTER A WITH GRAVE;Lu;0;L;0041 0300;;;;N;LATIN CAPITAL LETTER A GRAVE;;;00E0;
à	LATIN SMALL LETTER A WITH GRAVE;Ll;0;L;0061 0300;;;;N;LATIN SMALL LETTER A GRAVE;;00C0;;00C0

Α	GREEK CAPITAL LETTER ALPHA;Lu;0;L;;;;;N;;;;03B1;
α	GREEK SMALL LETTER ALPHA;Ll;0;L;;;;;N;;;0391;;0391
α	GREEK SMALL LETTER ALPHA;Ll;0;L;;;;;N;;;0391;;0391
β	GREEK SMALL LETTER BETA;Ll;0;L;;;;;N;;;0392;;0392
γ	GREEK SMALL LETTER GAMMA;Ll;0;L;;;;;N;;;0393;;0393
δ	GREEK SMALL LETTER DELTA;Ll;0;L;;;;;N;;;0394;;0394

Ⴀ	GEORGIAN CAPITAL LETTER AN;Lu;0;L;;;;;N;;Khutsuri;;;
ა	GEORGIAN LETTER AN;Lo;0;L;;;;;N;GEORGIAN SMALL LETTER AN;;;;

Ϣ	COPTIC CAPITAL LETTER SHEI;Lu;0;L;;;;;N;GREEK CAPITAL LETTER SHEI;;;03E3;
ϣ	COPTIC SMALL LETTER SHEI;Ll;0;L;;;;;N;GREEK SMALL LETTER SHEI;;03E2;;03E2

А	CYRILLIC CAPITAL LETTER A;Lu;0;L;;;;;N;;;;0430;
а	CYRILLIC SMALL LETTER A;Ll;0;L;;;;;N;;;0410;;0410

Ա	ARMENIAN CAPITAL LETTER AYB;Lu;0;L;;;;;N;;;;0561;
ա	ARMENIAN SMALL LETTER AYB;Ll;0;L;;;;;N;;;0531;;0531

𝐴	MATHEMATICAL ITALIC CAPITAL A;Lu;0;L;<font> 0041;;;;N;;;;;
𝑎	MATHEMATICAL ITALIC SMALL A;Ll;0;L;<font> 0061;;;;N;;;;;

𝔸	MATHEMATICAL DOUBLE-STRUCK CAPITAL A;Lu;0;L;<font> 0041;;;;N;;;;;
𝕒	MATHEMATICAL DOUBLE-STRUCK SMALL A;Ll;0;L;<font> 0061;;;;N;;;;;

-- title case letters
ǅ	LATIN CAPITAL LETTER D WITH SMALL LETTER Z WITH CARON;Lt;0;L;<compat> 0044 017E;;;;N;LATIN LETTER CAPITAL D SMALL Z HACEK;;01C4;01C6;01C5
ǈ	LATIN CAPITAL LETTER L WITH SMALL LETTER J;Lt;0;L;<compat> 004C 006A;;;;N;LATIN LETTER CAPITAL L SMALL J;;01C7;01C9;01C8

-- small caps
ᴀ	LATIN LETTER SMALL CAPITAL A;Ll;0;L;;;;;N;;;;;
ᴦ	GREEK LETTER SMALL CAPITAL GAMMA;Ll;0;L;;;;;N;;;;;

-- caseless letters
ଅ	;ORIYA LETTER A;Lo;0;L;;;;;N;;;;;
அ	TAMIL LETTER A;Lo;0;L;;;;;N;;;;;
అ	TELUGU LETTER A;Lo;0;L;;;;;N;;;;;
ಅ	KANNADA LETTER A;Lo;0;L;;;;;N;;;;;
അ	MALAYALAM LETTER A;Lo;0;L;;;;;N;;;;;
අ	SINHALA LETTER AYANNA;Lo;0;L;;;;;N;;;;;
ก	THAI CHARACTER KO KAI;Lo;0;L;;;;;N;THAI LETTER KO KAI;;;;
ກ	LAO LETTER KO;Lo;0;L;;;;;N;;;;;
ཀ	TIBETAN LETTER KA;Lo;0;L;;;;;N;;;;;
က	MYANMAR LETTER KA;Lo;0;L;;;;;N;;;;;
ᄀ	HANGUL CHOSEONG KIYEOK;Lo;0;L;;;;;N;;g *;;;
ሀ	ETHIOPIC SYLLABLE HA;Lo;0;L;;;;;N;;;;;
Ꭰ	CHEROKEE LETTER A;Lo;0;L;;;;;N;;;;;
ᐁ	CANADIAN SYLLABICS E;Lo;0;L;;;;;N;;;;;
ᚁ	OGHAM LETTER BEITH;Lo;0;L;;;;;N;;;;;
ᚠ	RUNIC LETTER FEHU FEOH FE F;Lo;0;L;;;;;N;;;;;
ᜀ	TAGALOG LETTER A;Lo;0;L;;;;;N;;;;;
ᜠ	HANUNOO LETTER A;Lo;0;L;;;;;N;;;;;
ᝀ	BUHID LETTER A;Lo;0;L;;;;;N;;;;;
ᝠ	TAGBANWA LETTER A;Lo;0;L;;;;;N;;;;;
ក	KHMER LETTER KA;Lo;0;L;;;;;N;;;;;
ᠠ	MONGOLIAN LETTER A;Lo;0;L;;;;;N;;;;;
ᤁ	LIMBU LETTER KA;Lo;0;L;;;;;N;;;;;
ᥐ	TAI LE LETTER KA;Lo;0;L;;;;;N;;;;;
ぁ	HIRAGANA LETTER SMALL A;Lo;0;L;;;;;N;;;;;
ア	KATAKANA LETTER A;Lo;0;L;;;;;N;;;;;
ㄅ	BOPOMOFO LETTER B;Lo;0;L;;;;;N;;;;;
ㄱ	HANGUL LETTER KIYEOK;Lo;0;L;<compat> 1100;;;;N;HANGUL LETTER GIYEOG;;;;
ㆠ	BOPOMOFO LETTER BU;Lo;0;L;;;;;N;;;;;
ꀀ	YI SYLLABLE IT;Lo;0;L;;;;;N;;;;;

-- spaces
 	NO-BREAK SPACE;Zs;0;CS;<noBreak> 0020;;;;N;NON-BREAKING SPACE;;;;
 	EN QUAD;Zs;0;WS;2002;;;;N;;;;;
 	EN SPACE;Zs;0;WS;<compat> 0020;;;;N;;;;;
 	THIN SPACE;Zs;0;WS;<compat> 0020;;;;N;;;;;
​	ZERO WIDTH SPACE;Zs;0;BN;;;;;N;;;;;

-- some symbols we might find useful in Haskell
←	LEFTWARDS ARROW;Sm;0;ON;;;;;N;LEFT ARROW;;;;
→	RIGHTWARDS ARROW;Sm;0;ON;;;;;N;RIGHT ARROW;;;;
‖	DOUBLE VERTICAL LINE;Po;0;ON;;;;;N;DOUBLE VERTICAL BAR;;;;
∀	FOR ALL;Sm;0;ON;;;;;N;;;;;
∁	COMPLEMENT;Sm;0;ON;;;;;Y;;;;;
∃	THERE EXISTS;Sm;0;ON;;;;;Y;;;;;
∄	THERE DOES NOT EXIST;Sm;0;ON;2203 0338;;;;Y;;;;;
∅	EMPTY SET;Sm;0;ON;;;;;N;;;;;
∆	INCREMENT;Sm;0;ON;;;;;N;;;;;
∇	NABLA;Sm;0;ON;;;;;N;;;;;
∈	ELEMENT OF;Sm;0;ON;;;;;Y;;;;;
∉	NOT AN ELEMENT OF;Sm;0;ON;2208 0338;;;;Y;;;;;
∏	N-ARY PRODUCT;Sm;0;ON;;;;;N;;;;;
∑	N-ARY SUMMATION;Sm;0;ON;;;;;Y;;;;;
−	MINUS SIGN;Sm;0;ET;;;;;N;;;;;
∓	MINUS-OR-PLUS SIGN;Sm;0;ET;;;;;N;;;;;
∕	DIVISION SLASH;Sm;0;ON;;;;;Y;;;;;
∘	RING OPERATOR;Sm;0;ON;;;;;N;;;;;
∙	BULLET OPERATOR;Sm;0;ON;;;;;N;;;;;
√	SQUARE ROOT;Sm;0;ON;;;;;Y;;;;;
∧	LOGICAL AND;Sm;0;ON;;;;;N;;;;;
∨	LOGICAL OR;Sm;0;ON;;;;;N;;;;;
∩	INTERSECTION;Sm;0;ON;;;;;N;;;;;
∪	UNION;Sm;0;ON;;;;;N;;;;;
≃	ASYMPTOTICALLY EQUAL TO;Sm;0;ON;;;;;Y;;;;;
≈	ALMOST EQUAL TO;Sm;0;ON;;;;;Y;;;;;
≠	NOT EQUAL TO;Sm;0;ON;003D 0338;;;;Y;;;;;
≙	ESTIMATES;Sm;0;ON;;;;;N;;;;;
≤	LESS-THAN OR EQUAL TO;Sm;0;ON;;;;;Y;LESS THAN OR EQUAL TO;;;;
≥	GREATER-THAN OR EQUAL TO;Sm;0;ON;;;;;Y;GREATER THAN OR EQUAL TO;;;;
≪	MUCH LESS-THAN;Sm;0;ON;;;;;Y;MUCH LESS THAN;;;;
≫	MUCH GREATER-THAN;Sm;0;ON;;;;;Y;MUCH GREATER THAN;;;;
⊂	SUBSET OF;Sm;0;ON;;;;;Y;;;;;
⊃	SUPERSET OF;Sm;0;ON;;;;;Y;;;;;
⊄	NOT A SUBSET OF;Sm;0;ON;2282 0338;;;;Y;;;;;
⊅	NOT A SUPERSET OF;Sm;0;ON;2283 0338;;;;Y;;;;;
⊆	SUBSET OF OR EQUAL TO;Sm;0;ON;;;;;Y;;;;;
⊇	SUPERSET OF OR EQUAL TO;Sm;0;ON;;;;;Y;;;;;
⊕	CIRCLED PLUS;Sm;0;ON;;;;;N;;;;;
⊖	CIRCLED MINUS;Sm;0;ON;;;;;N;;;;;
⊗	CIRCLED TIMES;Sm;0;ON;;;;;N;;;;;
⊘	CIRCLED DIVISION SLASH;Sm;0;ON;;;;;Y;;;;;
⊙	CIRCLED DOT OPERATOR;Sm;0;ON;;;;;N;;;;;
⊢	RIGHT TACK;Sm;0;ON;;;;;Y;;;;;
⊣	LEFT TACK;Sm;0;ON;;;;;Y;;;;;
⊤	DOWN TACK;Sm;0;ON;;;;;N;;;;;
⊥	UP TACK;Sm;0;ON;;;;;N;;;;;
⊦	ASSERTION;Sm;0;ON;;;;;Y;;;;;
⊧	MODELS;Sm;0;ON;;;;;Y;;;;;
⊨	TRUE;Sm;0;ON;;;;;Y;;;;;
⋂	N-ARY INTERSECTION;Sm;0;ON;;;;;N;;;;;
⋃	N-ARY UNION;Sm;0;ON;;;;;N;;;;;
⋅	DOT OPERATOR;Sm;0;ON;;;;;N;;;;;
⋯	MIDLINE HORIZONTAL ELLIPSIS;Sm;0;ON;;;;;N;;;;;
〈	LEFT-POINTING ANGLE BRACKET;Ps;0;ON;3008;;;;Y;BRA;;;;
〉	RIGHT-POINTING ANGLE BRACKET;Pe;0;ON;3009;;;;Y;KET;;;;
☹	WHITE FROWNING FACE;So;0;ON;;;;;N;;;;;
☺	WHITE SMILING FACE;So;0;ON;;;;;N;;;;;
⧺	DOUBLE PLUS;Sm;0;ON;;;;;N;;;;;

-- other random symbols
☣	BIOHAZARD SIGN;So;0;ON;;;;;N;;;;;
𝄬	MUSICAL SYMBOL FLAT UP;So;0;L;;;;;N;;;;;
𝌋	TETRAGRAM FOR CONTRARIETY;So;0;ON;;;;;N;;;;;

-- braille
⡍	;BRAILLE PATTERN DOTS-1347;So;0;ON;;;;;N;;;;;
⣿	;BRAILLE PATTERN DOTS-12345678;So;0;ON;;;;;N;;;;;

-- numbers
Ⅰ	;ROMAN NUMERAL ONE;Nl;0;L;<compat> 0049;;;1;N;;;;2170;
Ⅼ	;ROMAN NUMERAL FIFTY;Nl;0;L;<compat> 004C;;;50;N;;;;217C;
①	;CIRCLED DIGIT ONE;No;0;EN;<circle> 0031;;1;1;N;;;;;
⑴	;PARENTHESIZED DIGIT ONE;No;0;EN;<compat> 0028 0031 0029;;1;1;N;;;;;
⒈	;DIGIT ONE FULL STOP;No;0;EN;<compat> 0031 002E;;1;1;N;DIGIT ONE PERIOD;;;;
-}

module Main where

-- Test upper-case recognition:
data T 
  = À		-- latin
  | Α		-- greek
  | Ⴀ		-- georgian
  | Ϣ		-- coptic
  | А		-- cyrillic
  | Ա		-- armenian
  | 𝐴	-- maths italic
  | 𝔸	-- maths double-struck
  | ǅ		-- title case latin

-- Test lower-case recognition:
à α ϣ а ա 𝑎 𝕒 ᴀ ᴦ = undefined

-- Caseless characters in a string:
string = "ଅஅఅಅഅඅกກཀကᄀሀᎠᐁᚁᚠᜀᜠᝀᝠកᠠᤁᥐぁアㄅㄱㆠ" -- 29 chars

-- composition using a ring, greek type variables, and right arrows
(∘) :: ∀ α β γ . (β → γ) → (α → β) → (α → γ)
(f ∘ g) x = f (g x)

main = print ∘ length $ string
