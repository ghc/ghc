/*-----------------------------------------------------------------------------
  ctype.c for Haskell

  (c) Simon Marlow 1993
-----------------------------------------------------------------------------*/

#include "ctypes.h"

const unsigned char char_types[] = 
  {
    0,				/* \000 */
    0,				/* \001 */
    0,				/* \002 */
    0,				/* \003 */
    0,				/* \004 */
    0,				/* \005 */
    0,				/* \006 */
    0,				/* \007 */
    0,				/* \010 */
    C_Any | C_Space,   	  	/* \t */
    C_Any | C_Space,		/* \n */
    C_Any | C_Space,		/* \v */
    C_Any | C_Space,		/* \f */
    C_Any | C_Space,		/* ^M */
    0,				/* \016 */
    0,				/* \017 */
    0,				/* \020 */
    0,				/* \021 */
    0,				/* \022 */
    0,				/* \023 */
    0,				/* \024 */
    0,				/* \025 */
    0,				/* \026 */
    0,				/* \027 */
    0,				/* \030 */
    0,				/* \031 */
    0,				/* \032 */
    0,				/* \033 */
    0,				/* \034 */
    0,				/* \035 */
    0,				/* \036 */
    0,				/* \037 */
    C_Any | C_Space,		/*   */
    C_Any | C_Symbol,		/* ! */
    C_Any,			/* " */
    C_Any | C_Symbol,		/* # */
    C_Any | C_Symbol,		/* $ */
    C_Any | C_Symbol,		/* % */
    C_Any | C_Symbol,		/* & */
    C_Any | C_Ident,		/* ' */
    C_Any,			/* ( */
    C_Any,			/* ) */
    C_Any | C_Symbol,		/* * */
    C_Any | C_Symbol,		/* + */
    C_Any,			/* , */
    C_Any | C_Symbol,           /* - */
    C_Any | C_Symbol,		/* . */
    C_Any | C_Symbol,		/* / */
    C_Any | C_Ident | C_Digit,	/* 0 */
    C_Any | C_Ident | C_Digit,	/* 1 */
    C_Any | C_Ident | C_Digit,	/* 2 */
    C_Any | C_Ident | C_Digit,	/* 3 */
    C_Any | C_Ident | C_Digit,	/* 4 */
    C_Any | C_Ident | C_Digit,	/* 5 */
    C_Any | C_Ident | C_Digit,	/* 6 */
    C_Any | C_Ident | C_Digit,	/* 7 */
    C_Any | C_Ident | C_Digit,	/* 8 */
    C_Any | C_Ident | C_Digit,	/* 9 */
    C_Any | C_Symbol,		/* : */
    C_Any,			/* ; */
    C_Any | C_Symbol,		/* < */
    C_Any | C_Symbol,		/* = */
    C_Any | C_Symbol,		/* > */
    C_Any | C_Symbol,		/* ? */
    C_Any | C_Symbol,		/* @ */
    C_Any | C_Ident | C_Upper,	/* A */
    C_Any | C_Ident | C_Upper,	/* B */
    C_Any | C_Ident | C_Upper,	/* C */
    C_Any | C_Ident | C_Upper,	/* D */
    C_Any | C_Ident | C_Upper,	/* E */
    C_Any | C_Ident | C_Upper,	/* F */
    C_Any | C_Ident | C_Upper,	/* G */
    C_Any | C_Ident | C_Upper,	/* H */
    C_Any | C_Ident | C_Upper,	/* I */
    C_Any | C_Ident | C_Upper,	/* J */
    C_Any | C_Ident | C_Upper,	/* K */
    C_Any | C_Ident | C_Upper,	/* L */
    C_Any | C_Ident | C_Upper,	/* M */
    C_Any | C_Ident | C_Upper,	/* N */
    C_Any | C_Ident | C_Upper,	/* O */
    C_Any | C_Ident | C_Upper,	/* P */
    C_Any | C_Ident | C_Upper,	/* Q */
    C_Any | C_Ident | C_Upper,	/* R */
    C_Any | C_Ident | C_Upper,	/* S */
    C_Any | C_Ident | C_Upper,	/* T */
    C_Any | C_Ident | C_Upper,	/* U */
    C_Any | C_Ident | C_Upper,	/* V */
    C_Any | C_Ident | C_Upper,	/* W */
    C_Any | C_Ident | C_Upper,	/* X */
    C_Any | C_Ident | C_Upper,	/* Y */
    C_Any | C_Ident | C_Upper,	/* Z */
    C_Any,			/* [ */
    C_Any | C_Symbol,		/* \ */
    C_Any,			/* ] */
    C_Any | C_Symbol,		/* ^ */
    C_Any | C_Ident | C_Lower,	/* _ */
    C_Any,			/* ` */
    C_Any | C_Ident | C_Lower,	/* a */
    C_Any | C_Ident | C_Lower,	/* b */
    C_Any | C_Ident | C_Lower,	/* c */
    C_Any | C_Ident | C_Lower,	/* d */
    C_Any | C_Ident | C_Lower,	/* e */
    C_Any | C_Ident | C_Lower,	/* f */
    C_Any | C_Ident | C_Lower,	/* g */
    C_Any | C_Ident | C_Lower,	/* h */
    C_Any | C_Ident | C_Lower,	/* i */
    C_Any | C_Ident | C_Lower,	/* j */
    C_Any | C_Ident | C_Lower,	/* k */
    C_Any | C_Ident | C_Lower,	/* l */
    C_Any | C_Ident | C_Lower,	/* m */
    C_Any | C_Ident | C_Lower,	/* n */
    C_Any | C_Ident | C_Lower,	/* o */
    C_Any | C_Ident | C_Lower,	/* p */
    C_Any | C_Ident | C_Lower,	/* q */
    C_Any | C_Ident | C_Lower,	/* r */
    C_Any | C_Ident | C_Lower,	/* s */
    C_Any | C_Ident | C_Lower,	/* t */
    C_Any | C_Ident | C_Lower,	/* u */
    C_Any | C_Ident | C_Lower,	/* v */
    C_Any | C_Ident | C_Lower,	/* w */
    C_Any | C_Ident | C_Lower,	/* x */
    C_Any | C_Ident | C_Lower,	/* y */
    C_Any | C_Ident | C_Lower,	/* z */
    C_Any,			/* { */
    C_Any | C_Symbol,		/* | */
    C_Any,			/* } */
    C_Any | C_Symbol,		/* ~ */
    0,				/* \177 */
    0,				/* \200 */
    0,				/* \201 */
    0,				/* \202 */
    0,				/* \203 */
    0,				/* \204 */
    0,				/* \205 */
    0,				/* \206 */
    0,				/* \207 */
    0,				/* \210 */
    0,				/* \211 */
    0,				/* \212 */
    0,				/* \213 */
    0,				/* \214 */
    0,				/* \215 */
    0,				/* \216 */
    0,				/* \217 */
    0,				/* \220 */
    0,				/* \221 */
    0,				/* \222 */
    0,				/* \223 */
    0,				/* \224 */
    0,				/* \225 */
    0,				/* \226 */
    0,				/* \227 */
    0,				/* \230 */
    0,				/* \231 */
    0,				/* \232 */
    0,				/* \233 */
    0,				/* \234 */
    0,				/* \235 */
    0,				/* \236 */
    0,				/* \237 */
    C_Space,			/*   */
    C_Any | C_Symbol,		/* ¡ */
    C_Any | C_Symbol,		/* ¢ */
    C_Any | C_Symbol,		/* £ */
    C_Any | C_Symbol,		/* ¤ */
    C_Any | C_Symbol,		/* ¥ */
    C_Any | C_Symbol,		/* ¦ */
    C_Any | C_Symbol,		/* § */
    C_Any | C_Symbol,		/* ¨ */
    C_Any | C_Symbol,		/* © */
    C_Any | C_Symbol,		/* ª */
    C_Any | C_Symbol,		/* « */
    C_Any | C_Symbol,		/* ¬ */
    C_Any | C_Symbol,		/* ­ */
    C_Any | C_Symbol,		/* ® */
    C_Any | C_Symbol,		/* ¯ */
    C_Any | C_Symbol,		/* ° */
    C_Any | C_Symbol,		/* ± */
    C_Any | C_Symbol,		/* ² */
    C_Any | C_Symbol,		/* ³ */
    C_Any | C_Symbol,		/* ´ */
    C_Any | C_Symbol,		/* µ */
    C_Any | C_Symbol,		/* ¶ */
    C_Any | C_Symbol,		/* · */
    C_Any | C_Symbol,		/* ¸ */
    C_Any | C_Symbol,		/* ¹ */
    C_Any | C_Symbol,		/* º */
    C_Any | C_Symbol,		/* » */
    C_Any | C_Symbol,		/* ¼ */
    C_Any | C_Symbol,		/* ½ */
    C_Any | C_Symbol,		/* ¾ */
    C_Any | C_Symbol,		/* ¿ */
    C_Any | C_Ident | C_Upper,	/* À */
    C_Any | C_Ident | C_Upper,	/* Á */
    C_Any | C_Ident | C_Upper,	/* Â */
    C_Any | C_Ident | C_Upper,	/* Ã */
    C_Any | C_Ident | C_Upper,	/* Ä */
    C_Any | C_Ident | C_Upper,	/* Å */
    C_Any | C_Ident | C_Upper,	/* Æ */
    C_Any | C_Ident | C_Upper,	/* Ç */
    C_Any | C_Ident | C_Upper,	/* È */
    C_Any | C_Ident | C_Upper,	/* É */
    C_Any | C_Ident | C_Upper,	/* Ê */
    C_Any | C_Ident | C_Upper,	/* Ë */
    C_Any | C_Ident | C_Upper,	/* Ì */
    C_Any | C_Ident | C_Upper,	/* Í */
    C_Any | C_Ident | C_Upper,	/* Î */
    C_Any | C_Ident | C_Upper,	/* Ï */
    C_Any | C_Ident | C_Upper,	/* Ð */
    C_Any | C_Ident | C_Upper,	/* Ñ */
    C_Any | C_Ident | C_Upper,	/* Ò */
    C_Any | C_Ident | C_Upper,	/* Ó */
    C_Any | C_Ident | C_Upper,	/* Ô */
    C_Any | C_Ident | C_Upper,	/* Õ */
    C_Any | C_Ident | C_Upper,	/* Ö */
    C_Any | C_Symbol | C_Lower,	/* × */
    C_Any | C_Ident | C_Upper,	/* Ø */
    C_Any | C_Ident | C_Upper,	/* Ù */
    C_Any | C_Ident | C_Upper,	/* Ú */
    C_Any | C_Ident | C_Upper,	/* Û */
    C_Any | C_Ident | C_Upper,	/* Ü */
    C_Any | C_Ident | C_Upper,	/* Ý */
    C_Any | C_Ident | C_Upper,	/* Þ */
    C_Any | C_Ident,		/* ß */
    C_Any | C_Ident | C_Lower,	/* à */
    C_Any | C_Ident | C_Lower,	/* á */
    C_Any | C_Ident | C_Lower,	/* â */
    C_Any | C_Ident | C_Lower,	/* ã */
    C_Any | C_Ident | C_Lower,	/* ä */
    C_Any | C_Ident | C_Lower,	/* å */
    C_Any | C_Ident | C_Lower,	/* æ */
    C_Any | C_Ident | C_Lower,	/* ç */
    C_Any | C_Ident | C_Lower,	/* è */
    C_Any | C_Ident | C_Lower,	/* é */
    C_Any | C_Ident | C_Lower,	/* ê */
    C_Any | C_Ident | C_Lower,	/* ë */
    C_Any | C_Ident | C_Lower,	/* ì */
    C_Any | C_Ident | C_Lower,	/* í */
    C_Any | C_Ident | C_Lower,	/* î */
    C_Any | C_Ident | C_Lower,	/* ï */
    C_Any | C_Ident | C_Lower,	/* ð */
    C_Any | C_Ident | C_Lower,	/* ñ */
    C_Any | C_Ident | C_Lower,	/* ò */
    C_Any | C_Ident | C_Lower,	/* ó */
    C_Any | C_Ident | C_Lower,	/* ô */
    C_Any | C_Ident | C_Lower,	/* õ */
    C_Any | C_Ident | C_Lower,	/* ö */
    C_Any | C_Symbol,		/* ÷ */
    C_Any | C_Ident,		/* ø */
    C_Any | C_Ident | C_Lower,	/* ù */
    C_Any | C_Ident | C_Lower,	/* ú */
    C_Any | C_Ident | C_Lower,	/* û */
    C_Any | C_Ident | C_Lower,	/* ü */
    C_Any | C_Ident | C_Lower,	/* ý */
    C_Any | C_Ident | C_Lower,	/* þ */
    C_Any | C_Ident | C_Lower,	/* ÿ */
  };
