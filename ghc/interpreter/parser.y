/* -*- mode: hugs-c; -*- */
/* --------------------------------------------------------------------------
 * Hugs parser (included as part of input.c)
 *
 * Copyright (c) The University of Nottingham and Yale University, 1994-1997.
 * All rights reserved. See NOTICE for details and conditions of use etc...
 * Hugs version 1.4, December 1997
 *
 * Expect 24 shift/reduce conflicts when passing this grammar through yacc,
 * but don't worry; they will all be resolved in an appropriate manner.
 *
 * $RCSfile: parser.y,v $
 * $Revision: 1.2 $
 * $Date: 1998/12/02 13:22:26 $
 * ------------------------------------------------------------------------*/

%{
#ifndef lint
#define lint
#endif
#define defTycon(n,l,lhs,rhs,w)  tyconDefn(intOf(l),lhs,rhs,w); sp-=n
#define sigdecl(l,vs,t)          ap(SIGDECL,triple(l,vs,t))
#define grded(gs)                ap(GUARDED,gs)
#define bang(t)                  ap(BANG,t)
#define only(t)                  ap(ONLY,t)
#define letrec(bs,e)             (nonNull(bs) ? ap(LETREC,pair(bs,e)) : e)
#define exportSelf()             singleton(ap(MODULEENT,mkCon(module(currentModule).text)))
#define yyerror(s)               /* errors handled elsewhere */
#define YYSTYPE                  Cell

static Cell   local gcShadow     Args((Int,Cell));
static Void   local syntaxError  Args((String));
static String local unexpected   Args((Void));
static Cell   local checkPrec    Args((Cell));
static Void   local fixDefn      Args((Syntax,Cell,Cell,List));
static Void   local setSyntax    Args((Int,Syntax,Cell));
static Cell   local buildTuple   Args((List));
static List   local checkContext Args((List));
static Cell   local checkPred    Args((Cell));
static Pair   local checkDo      Args((List));
static Cell   local checkTyLhs   Args((Cell));
#if !TREX
static Void   local noTREX       Args((String));
#endif
static Cell   local tidyInfix    Args((Cell));

/* For the purposes of reasonably portable garbage collection, it is
 * necessary to simulate the YACC stack on the Hugs stack to keep
 * track of all intermediate constructs.  The lexical analyser
 * pushes a token onto the stack for each token that is found, with
 * these elements being removed as reduce actions are performed,
 * taking account of look-ahead tokens as described by gcShadow()
 * below.
 *
 * Of the non-terminals used below, only start, topDecl, fixDecl & begin
 * do not leave any values on the Hugs stack.  The same is true for the
 * terminals EXPR and SCRIPT.  At the end of a successful parse, there
 * should only be one element left on the stack, containing the result
 * of the parse.
 */

#define gc0(e)                   gcShadow(0,e)
#define gc1(e)                   gcShadow(1,e)
#define gc2(e)                   gcShadow(2,e)
#define gc3(e)                   gcShadow(3,e)
#define gc4(e)                   gcShadow(4,e)
#define gc5(e)                   gcShadow(5,e)
#define gc6(e)                   gcShadow(6,e)
#define gc7(e)                   gcShadow(7,e)

%}

%token EXPR       SCRIPT
%token CASEXP     OF         DATA       TYPE       IF
%token THEN       ELSE       WHERE      LET        IN
%token INFIX      INFIXL     INFIXR     FOREIGN    TNEWTYPE
%token DEFAULT    DERIVING   DO         TCLASS     TINSTANCE
%token REPEAT     ALL
%token VAROP      VARID      NUMLIT     CHARLIT    STRINGLIT
%token CONOP      CONID
%token QVAROP     QVARID     QCONOP     QCONID
/*#if TREX*/
%token RECSELID
/*#endif*/
%token COCO       '='        UPTO       '@'        '\\'
%token '|'        '-'        FROM       ARROW      '~'
%token '!'        IMPLIES    '('        ','        ')'
%token '['        ';'        ']'        '`'        '.'
%token MODULETOK  IMPORT     HIDING     QUALIFIED  ASMOD
%token EXPORT     INTERFACE  REQUIRES   UNSAFE

%%
/*- Top level script/module structure: ------------------------------------*/

start     : EXPR exp wherePart          {inputExpr = letrec($3,$2); sp-=2;}
          | SCRIPT topModule            {valDefns  = $2;            sp-=1;}
          | INTERFACE iface             {sp-=1;}
          | error                       {syntaxError("input");}
          ;

/*- GHC interface file parsing: -------------------------------------------*/

/* Reading in an interface file is surprisingly like reading
 * a normal Haskell module: we read in a bunch of declarations,
 * construct symbol table entries, etc.  The "only" differences
 * are that there's no syntactic sugar to deal with and we don't
 * have to read in expressions.
 */

iface     : INTERFACE ifaceName NUMLIT checkVersion ifaceDecls { $$ = gc5(NIL); }
          | INTERFACE error             {syntaxError("interface file");}
          ;

ifaceName : CONID                       {openGHCIface(textOf($1)); $$ = gc1(NIL);}
          ;

ifaceDecls:                             {$$=gc0(NIL);}
          | ifaceDecl ';' ifaceDecls    {$$=gc3(cons($1,$2));}
          ;

/* We use ifaceData in data decls so as to include () */
ifaceDecl : IMPORT CONID NUMLIT         { extern String scriptFile;
                                          String fileName = findPathname(scriptFile,textToStr(textOf($2)));
                                          addGHCImport(intOf($1),textOf($2),fileName);                 
                                          $$ = gc3(NIL); 
                                        }
          | EXPORT CONID ifaceEntities  {}                                                          
          | REQUIRES STRINGLIT          { extern String scriptFile;
                                          String fileName = findPathname(scriptFile,textToStr(textOf($2)));
                                          loadSharedLib(fileName);                  
                                          $$ = gc2(NIL); 
                                        }
          | INFIXL optdigit op                                                   { fixDefn(LEFT_ASS,$1,$2,$3);                 $$ = gc3(NIL); }
          | INFIXR optdigit op                                                   { fixDefn(RIGHT_ASS,$1,$2,$3);                $$ = gc3(NIL); }
          | INFIX  optdigit op                                                   { fixDefn(NON_ASS,$1,$2,$3);                  $$ = gc3(NIL); }
          | TINSTANCE ifaceQuant ifaceClass '=' ifaceVar                         { addGHCInstance(intOf($1),$2,$3,textOf($5)); $$ = gc5(NIL); }
          | NUMLIT TYPE     ifaceTCName ifaceTVBndrs '=' ifaceType               { addGHCSynonym(intOf($2),$3,$4,$6);          $$ = gc6(NIL); }
          | NUMLIT DATA     ifaceData   ifaceTVBndrs ifaceConstrs ifaceSels      { addGHCDataDecl(intOf($2),$3,$4,$5,$6);      $$ = gc6(NIL); }
          | NUMLIT TNEWTYPE ifaceTCName ifaceTVBndrs ifaceNewTypeConstr          { addGHCNewType(intOf($2),$3,$4,$5);          $$ = gc5(NIL); }
          | NUMLIT TCLASS   ifaceDeclContext ifaceTCName ifaceTVBndrs ifaceCSigs { addGHCClass(intOf($2),$3,$4,$5,$6);         $$ = gc6(NIL); }
          | NUMLIT ifaceVar COCO ifaceType                                       { addGHCVar(intOf($3),textOf($2),$4);         $$ = gc4(NIL); }
          | error                                                                { syntaxError("interface declaration"); }
          ;

checkVersion
          : NUMLIT                      { $$ = gc1(NIL); }
          ;

ifaceSels /* [(VarId,Type)] */
          :                             { $$ = gc0(NIL); }
          | WHERE '{' ifaceSels1 '}'    { $$ = gc4($3); }
          ;

ifaceSels1 /* [(VarId,Type)] */
          : ifaceSel                    { $$ = gc1(singleton($1)); }
          | ifaceSel ';' ifaceSels1     { $$ = gc3(cons($1,$3)); }
          ;

ifaceSel /* (VarId,Type) */
          : ifaceVarName COCO ifaceType { $$ = gc3(pair($1,$3)); }
          ;

ifaceCSigs /* [(VarId,Type)] */
          :                             { $$ = gc0(NIL); }
          | WHERE '{' ifaceCSigs1 '}'   { $$ = gc4($3); }
          ;

ifaceCSigs1 /* [(VarId,Type)] */
          : ifaceCSig                   { $$ = gc1(singleton($1)); }
          | ifaceCSig ';' ifaceCSigs1   { $$ = gc3(cons($1,$3));    }
          ;

ifaceCSig /* (VarId,Type) */
          : ifaceVarName     COCO ifaceType { $$ = gc3(pair($1,$3)); }
          | ifaceVarName '=' COCO ifaceType { $$ = gc4(pair($1,$4)); } /* has default method */
          ;

ifaceConstrs /* [(ConId,[VarId],Type)] */
          :                             { $$ = gc0(NIL); }
          | '=' ifaceConstrs1           { $$ = gc2($2);  }
          ;

ifaceConstrs1 /* [(ConId,[VarId],Type)] */
          : ifaceConstr                   { $$ = gc1(singleton($1)); }
          | ifaceConstr '|' ifaceConstrs1 { $$ = gc3(cons($1,$3));   }
          ;

/* We use ifaceData so as to include () */
ifaceConstr /* (ConId,[VarId],Type) */
          : ifaceData                        COCO ifaceType { $$ = gc3(triple($1,NIL,$3)); }
          | ifaceData '{' ifaceVarNames1 '}' COCO ifaceType { $$ = gc6(triple($1,$3,$6));  }  
          ;

ifaceNewTypeConstr /* (ConId,Type) */
          :                                   { $$ = gc0(NIL);         }
          | '=' ifaceDataName COCO ifaceType  { $$ = gc4(pair($2,$4)); }
          ;

ifaceQuant /* Maybe ([(VarId,Kind)],[(ConId, [Type])]) */ 
          :                                      { $$ = gc0(NIL); }
          | ALL ifaceForall ifaceContext IMPLIES { $$ = gc4(pair($2,$3)); }
          ;

ifaceType
          : ALL ifaceForall ifaceContext IMPLIES ifaceType { $$ = gc5(ap(POLYTYPE,triple($2,$3,$5))); }
          | ifaceBType ARROW ifaceType          { $$ = gc3(fn($1,$3)); }
          | ifaceBType                          { $$ = gc1($1); }
          ;					
						
ifaceForall /* [(VarId,Kind)] */
          : '[' ifaceTVBndrs ']'                { $$ = gc3($2); }
          ;					
						
ifaceDeclContext /* [(ConId, [Type])] */ 
          :                                     { $$ = gc0(NIL); }
          | '{' ifaceContextList1 '}' IMPLIES   { $$ = gc4($2);  }
          ;					
						
ifaceContext /* [(ConId, [Type])] */				
          :                                     { $$ = gc0(NIL); }
          | '{' ifaceContextList1 '}'           { $$ = gc3($2);  }
          ;					
						
ifaceContextList1 /* [(ConId, [Type])] */			
          : ifaceClass                          { $$ = gc1(singleton($1)); }
          | ifaceClass ',' ifaceContextList1    { $$ = gc3(cons($1,$3));   }
          ;

ifaceClass /* (ConId, [Type]) */
          : ifaceQTCName ifaceATypes            { $$ = gc2(pair($1,$2)); }
          ;				        

ifaceTypes2
          : ifaceType ',' ifaceType             { $$ = gc3(doubleton($1,$3)); }
          | ifaceType ',' ifaceTypes2           { $$ = gc3(cons($1,$3));      }
          ;
					        
ifaceBType				        
          : ifaceAType                          { $$ = gc1($1);        } 
          | ifaceBType ifaceAType               { $$ = gc2(ap($1,$2)); }
          ;

ifaceAType				        
          : ifaceQTCName                        { $$ = gc1($1); }
          | ifaceTVName                         { $$ = gc1($1); }
          | '(' ')'                             { $$ = gc2(conPreludeUnit); }
          | '(' ifaceTypes2 ')'                 { $$ = gc3(buildTuple($2)); }
          | '[' ifaceType ']'                   { $$ = gc3(ap(conPreludeList,$2));}
          | '{' ifaceQTCName ifaceATypes '}'    { $$ = gc4(ap(DICTAP,pair($2,$3))); }
          | '(' ifaceType ')'                   { $$ = gc3($2); }
          ;

ifaceATypes
          :                                     { $$ = gc0(NIL);         }
          | ifaceAType ifaceATypes              { $$ = gc2(cons($1,$2)); }
          ;

ifaceEntities				        
          :                                     { $$ = gc0(NIL);         }
          | ifaceEntity ifaceEntities           { $$ = gc2(cons($1,$2)); }
          ;

ifaceEntity
          : ifaceEntityOcc                      {}
          | ifaceEntityOcc ifaceStuffInside     {}
| ifaceEntityOcc '|' ifaceStuffInside {} /* exporting datacons but not tycon */
          ;

ifaceEntityOcc
          : ifaceVar                    { $$ = gc1($1); }
          | ifaceData                   { $$ = gc1($1); }
          | ARROW                       { $$ = gc3(typeArrow); }
          | '(' ARROW ')'               { $$ = gc3(typeArrow); }  /* why allow both? */
          ;

ifaceStuffInside
          : '{' ifaceValOccs '}'        { $$ = gc1($1); }
          ;


ifaceValOccs
          : ifaceValOcc                 { $$ = gc1(singleton($1)); }
          | ifaceValOcc ifaceValOccs    { $$ = gc2(cons($1,$2));   }
          ;

ifaceValOcc
          : ifaceVar                    {$$ = gc1($1); }
          | ifaceData                   {$$ = gc1($1); }
          ;

ifaceVar  : VARID                       {$$ = gc1($1);      }
          | VAROP                       {$$ = gc1($1);      }
          | '!'                         {$$ = gc1(varBang); }
          | '.'                         {$$ = gc1(varDot);  }
          | '-'                         {$$ = gc1(varMinus);}
          ;

ifaceData /* ConId | QualConId */
          : CONID                       {$$ = gc1($1);}
          | CONOP                       {$$ = gc1($1);}
          | '(' ')'                     {$$ = gc2(conPreludeUnit);}
          | '[' ']'                     {$$ = gc2(conPreludeList);}
          ;

ifaceVarName /* VarId */
          : ifaceVar                    { $$ = gc1($1); }
          ;

ifaceDataName /* ConId|QualConId */
          : ifaceData                   { $$ = gc1($1); }
          ;

ifaceVarNames1 /* [VarId] */
          : ifaceVarName                { $$ = gc1(singleton($1)); }
          | ifaceVarName ifaceVarNames1 { $$ = gc2(cons($1,$2));   }
          ;

ifaceTVName /* VarId */
          : VARID                       { $$ = gc1($1); }
          ; 

ifaceTVBndrs /* [(VarId,Kind)] */
          :                             { $$ = gc0(NIL);         }
          | ifaceTVBndr ifaceTVBndrs    { $$ = gc2(cons($1,$2)); }
          ;

ifaceTVBndr /* (VarId,Kind) */
          : ifaceTVName                 { $$ = gc1(pair($1,STAR)); }
          | ifaceTVName COCO ifaceAKind { $$ = gc3(pair($1,$3));   }
          ; 

ifaceKind
          : ifaceAKind                  { $$ = gc1($1);        }
          | ifaceAKind ARROW ifaceKind  { $$ = gc3(fn($1,$3)); }
          ;

ifaceAKind
          : VAROP                       { $$ = gc1(STAR); } /* should be '*' */
          | '(' ifaceKind ')'           { $$ = gc1($1);   }
          ;

ifaceTCName
          : CONID                       { $$ = gc1($1); }
          | CONOP                       { $$ = gc1($1); }
          | '(' ARROW ')'               { $$ = gc3(typeArrow); }
          | '[' ']'                     { $$ = gc1(conPreludeList);  }
          ; 

ifaceQTCName
          : ifaceTCName                 { $$ = gc1($1); }
          | QCONID                      { $$ = gc1($1); }
          | QCONOP                      { $$ = gc1($1); }
          ; 

/*- Haskell module header/import parsing: ---------------------------------*/

/* In Haskell 1.2, the default module header was "module Main where"
 * In 1.3, this changed to "module Main(main) where".
 * We use the 1.2 header because it breaks much less pre-module code.
 */
topModule : startMain begin modBody end {
                                         setExportList(singleton(ap(MODULEENT,mkCon(module(currentModule).text))));
                                         $$ = gc3($3);
                                        }
          | MODULETOK modname expspec WHERE '{' modBody end
                                        {setExportList($3);   $$ = gc7($6);}
          | MODULETOK error             {syntaxError("module definition");}
          ;
/* To implement the Haskell module system, we have to keep track of the
 * current module.  We rely on the use of LALR parsing to ensure that this 
 * side effect happens before any declarations within the module.
 */
startMain : /* empty */                 {startModule(conMain); 
                                         $$ = gc0(NIL);}
          ;
modname   : CONID                       {startModule($1); $$ = gc1(NIL);}
          ;
modid     : CONID                       {$$ = gc1($1);}
          | STRINGLIT                   { extern String scriptFile;
                                          String modName = findPathname(scriptFile,textToStr(textOf($1)));
                                          if (modName) { /* fillin pathname if known */
                                              $$ = mkStr(findText(modName));
                                          } else {
                                              $$ = $1;
                                          }
                                        }
          ;
modBody   : topDecls                    {$$ = gc1($1);}
          | fixDecls ';' topDecls       {$$ = gc3($3);}
          | impDecls chase              {$$ = gc2(NIL);}
          | impDecls ';' chase topDecls {$$ = gc4($4);}
          | impDecls ';' chase fixDecls ';' topDecls
                                        {$$ = gc6($6);}
          ;

/*- Exports: --------------------------------------------------------------*/

expspec   : /* empty */                 {$$ = gc0(exportSelf());}
          | '(' ')'                     {$$ = gc2(NIL);}
          | '(' exports ')'             {$$ = gc3($2);}
          | '(' exports ',' ')'         {$$ = gc4($2);}
          ;
exports   : exports ',' export          {$$ = gc3(cons($3,$1));}
          | export                      {$$ = gc1(singleton($1));}
          ;
/* The qcon should be qconid.  
 * Relaxing the rule lets us explicitly export (:) from the Prelude.
 */
export    : qvar                        {$$ = gc1($1);}
          | qcon                        {$$ = gc1($1);}
          | qcon2 '(' UPTO ')'          {$$ = gc4(pair($1,DOTDOT));}
          | qcon2 '(' qnames ')'        {$$ = gc4(pair($1,$3));}
          | MODULETOK modid             {$$ = gc2(ap(MODULEENT,$2));}
          ;
qnames    : /* empty */                 {$$ = gc0(NIL);}
          | ','                         {$$ = gc1(NIL);}
          | qnames1                     {$$ = gc1($1);}
          | qnames1 ','                 {$$ = gc2($1);}
          ;
qnames1   : qnames1 ',' qname           {$$ = gc3(cons($3,$1));}
          | qname                       {$$ = gc1(singleton($1));}
          ;
qname     : qvar                        {$$ = gc1($1);}
          | qcon                        {$$ = gc1($1);}
          | '(' ')'                     {$$ = gc2(conPreludeUnit);}
          | '[' ']'                     {$$ = gc2(conPreludeList);}
          ;
qcon2     : '(' ')'                     {$$ = gc2(conPreludeUnit);}
          | '[' ']'                     {$$ = gc2(conPreludeList);}
          | qconid                      {$$ = gc1($1);}
          ;

/*- Import declarations: --------------------------------------------------*/

impDecls  : impDecls ';' impDecl        {imps = cons($3,imps); $$=gc3(NIL);}
          | impDecl                     {imps = singleton($1); $$=gc1(NIL);}
          ;
chase     : /* empty */                 {if (chase(imps)) {
                                             clearStack();
                                             onto(imps);
                                             done();
                                             closeAnyInput();
                                             return 0;
                                         }
                                         $$ = gc0(NIL);
                                        }
          ;
/* Note that qualified import ignores the import list. */
impDecl   : IMPORT modid impspec        {addQualImport($2,$2);
                                         addUnqualImport($2,$3);
                                         $$ = gc3($2);}
          | IMPORT modid ASMOD modid impspec
                                        {addQualImport($2,$4);
                                         addUnqualImport($2,$5);
                                         $$ = gc5($2);}
          | IMPORT QUALIFIED modid ASMOD modid impspec
                                        {addQualImport($3,$5);
                                         $$ = gc6($3);}
          | IMPORT QUALIFIED modid impspec
                                        {addQualImport($3,$3);
                                         $$ = gc4($3);}
          | IMPORT error                {syntaxError("import declaration");}
          ;
impspec   : /* empty */                 {$$ = gc0(DOTDOT);}
          | HIDING '(' imports ')'      {$$ = gc4(ap(HIDDEN,$3));}
          | '(' imports ')'             {$$ = gc3($2);}
          ;
imports   : /* empty */                 {$$ = gc0(NIL);}
          | ','                         {$$ = gc1(NIL);}
          | imports1                    {$$ = gc1($1);}
          | imports1 ','                {$$ = gc2($1);}
          ;
imports1  : imports1 ',' import         {$$ = gc3(cons($3,$1));}
          | import                      {$$ = gc1(singleton($1));}
          ;
import    : var                         {$$ = gc1($1);}
          | CONID                       {$$ = gc1($1);}
          | CONID '(' UPTO ')'          {$$ = gc4(pair($1,DOTDOT));}
          | CONID '(' names ')'         {$$ = gc4(pair($1,$3));}
          ;
names     : /* empty */                 {$$ = gc0(NIL);}
          | ','                         {$$ = gc1(NIL);}
          | names1                      {$$ = gc1($1);}
          | names1 ','                  {$$ = gc2($1);}
          ;
names1    : names1 ',' name             {$$ = gc3(cons($3,$1));}
          | name                        {$$ = gc1(singleton($1));}
          ;
name      : var                         {$$ = gc1($1);}
          | con                       {$$ = gc1($1);}
          ;

/*- Fixity declarations: --------------------------------------------------*/

fixDecls  : fixDecls ';' fixDecl        {$$ = gc2(NIL);}
          | fixDecl                     {$$ = gc0(NIL);}
          ;
fixDecl   : INFIXL optdigit ops         {fixDefn(LEFT_ASS,$1,$2,$3); sp-=3;}
          | INFIXR optdigit ops         {fixDefn(RIGHT_ASS,$1,$2,$3);sp-=3;}
          | INFIX  optdigit ops         {fixDefn(NON_ASS,$1,$2,$3);  sp-=3;}
          ;
optdigit  : NUMLIT                      {$$ = gc1(checkPrec($1));}
          | /* empty */                 {$$ = gc0(mkInt(DEF_PREC));}
          ;
ops       : ops ',' op                  {$$ = gc3(cons($3,$1));}
          | op                          {$$ = gc1(cons($1,NIL));}
          ;

/*- Top-level declarations: -----------------------------------------------*/

topDecls  : /* empty */                 {$$ = gc0(NIL);}
          | ';'                         {$$ = gc1(NIL);}
          | topDecls1                   {$$ = gc1($1);}
          | topDecls1 ';'               {$$ = gc2($1);}
          ;
topDecls1 : topDecls1 ';' topDecl       {$$ = gc2($1);}
          | topDecls1 ';' decl          {$$ = gc3(cons($3,$1));}
          | topDecl                     {$$ = gc0(NIL);}
          | decl                        {$$ = gc1(cons($1,NIL));}
          ;

/*- Type declarations: ----------------------------------------------------*/

topDecl   : TYPE tyLhs '=' type         {defTycon(4,$3,$2,$4,SYNONYM);}
          | TYPE tyLhs '=' type IN invars
                                        {defTycon(6,$3,$2,
                                                    ap($4,$6),RESTRICTSYN);}
          | DATA btype2 '=' constrs deriving
                                        {defTycon(5,$3,checkTyLhs($2),
                                                    ap(rev($4),$5),DATATYPE);}
          | DATA context IMPLIES tyLhs '=' constrs deriving
                                        {defTycon(7,$5,$4,
                                                  ap(ap(QUAL,pair($2,rev($6))),
                                                     $7),DATATYPE);}
          | DATA btype2                 {defTycon(2,$1,checkTyLhs($2),
                                                    ap(NIL,NIL),DATATYPE);}
          | DATA context IMPLIES tyLhs  {defTycon(4,$1,$4,
                                                  ap(ap(QUAL,pair($2,NIL)),
                                                     NIL),DATATYPE);}
          | TNEWTYPE btype2 '=' nconstr deriving
                                        {defTycon(5,$3,checkTyLhs($2),
                                                    ap($4,$5),NEWTYPE);}
          | TNEWTYPE context IMPLIES tyLhs '=' nconstr deriving
                                        {defTycon(7,$5,$4,
                                                  ap(ap(QUAL,pair($2,$6)),
                                                     $7),NEWTYPE);}
          ;
tyLhs     : tyLhs varid1                {$$ = gc2(ap($1,$2));}
          | CONID                       {$$ = gc1($1);}
          | '[' type ']'                {$$ = gc3(ap(conList,$2));}
          | '(' ')'                     {$$ = gc2(conUnit);}
          | '(' typeTuple ')'           {$$ = gc3(buildTuple($2));}
          | error                       {syntaxError("type defn lhs");}
          ;
invars    : invars ',' invar            {$$ = gc3(cons($3,$1));}
          | invar                       {$$ = gc1(cons($1,NIL));}
          ;
invar     : qvar COCO topType           {$$ = gc3(sigdecl($2,singleton($1),
                                                             $3));}
          | qvar                        {$$ = gc1($1);}
          ;
constrs   : constrs '|' constr          {$$ = gc3(cons($3,$1));}
          | constr                      {$$ = gc1(cons($1,NIL));}
          ;
constr    : '!' btype conop bbtype      {$$ = gc4(ap2($3,bang($2),$4));}
          | btype1    conop bbtype      {$$ = gc3(ap2($2,$1,$3));}
          | btype2    conop bbtype      {$$ = gc3(ap2($2,$1,$3));}
          | bpolyType conop bbtype      {$$ = gc3(ap2($2,$1,$3));}
          | btype2                      {$$ = gc1($1);}
          | btype3                      {$$ = gc1($1);}
          | btype4                      {$$ = gc1($1);}
          | con '{' fieldspecs '}'      {$$ = gc4(ap(LABC,pair($1,rev($3))));}
          | '[' ']'                     {$$ = gc2(conNil);}
          | '(' ')'                     {$$ = gc2(conUnit);}
          | '(' typeTuple ')'           {$$ = gc3(buildTuple($2));}
          | error                       {syntaxError("data type definition");}
          ;
btype3    : btype2 '!' atype            {$$ = gc3(ap($1,bang($3)));}
          | btype3 '!' atype            {$$ = gc3(ap($1,bang($3)));}
          | btype3 atype                {$$ = gc2(ap($1,$2));}
          ;
btype4    : btype2 bpolyType            {$$ = gc2(ap($1,$2));}
          | btype3 bpolyType            {$$ = gc2(ap($1,$2));}
          | btype4 bpolyType            {$$ = gc2(ap($1,$2));}
          | btype4 atype                {$$ = gc2(ap($1,$2));}
          | btype4 '!' atype            {$$ = gc3(ap($1,bang($3)));}
          ;
bbtype    : '!' btype                   {$$ = gc2(bang($2));}
          | btype                       {$$ = gc1($1);}
          | bpolyType                   {$$ = gc1($1);}
          ;
fieldspecs: fieldspecs ',' fieldspec    {$$ = gc3(cons($3,$1));}
          | fieldspec                   {$$ = gc1(cons($1,NIL));}
          ;
fieldspec : vars COCO polyType          {$$ = gc3(pair(rev($1),$3));}
          | vars COCO type              {$$ = gc3(pair(rev($1),$3));}
          ;
nconstr   : con atype                   {$$ = gc2(singleton(ap($1,$2)));}
          | con bpolyType               {$$ = gc2(singleton(ap($1,$2)));}
          ;
deriving  : /* empty */                 {$$ = gc0(NIL);}
          | DERIVING qconid             {$$ = gc2(singleton($2));}
          | DERIVING '(' derivs0 ')'    {$$ = gc4($3);}
          ;
derivs0   : /* empty */                 {$$ = gc0(NIL);}
          | derivs                      {$$ = gc1(rev($1));}
          ;
derivs    : derivs ',' qconid           {$$ = gc3(cons($3,$1));}
          | qconid                      {$$ = gc1(singleton($1));}
          ;

/*- Processing definitions of primitives ----------------------------------*/

topDecl   : FOREIGN IMPORT callconv ext_loc ext_name unsafe_flag var COCO type 
                                        {foreignImport($1,pair($4,$5),$7,$9); sp-=9;}
          | FOREIGN EXPORT callconv ext_name qvarid COCO type 
                                        {foreignExport($1,$4,$5,$7); sp-=7;}
	  ;

callconv  : var                  {$$ = gc1(NIL); /* ignored */ }
          ;
ext_loc   : STRINGLIT            {$$ = $1;}
          ;
ext_name  : STRINGLIT            {$$ = $1;}
          ;
unsafe_flag: /* empty */         {$$ = gc0(NIL);}
          | UNSAFE               {$$ = gc1(NIL); /* ignored */ }
          ;


/*- Class declarations: ---------------------------------------------------*/

topDecl   : TCLASS crule wherePart      {classDefn(intOf($1),$2,$3); sp-=3;}
          | TINSTANCE irule wherePart   {instDefn(intOf($1),$2,$3);  sp-=3;}
          | DEFAULT '(' dtypes ')'      {defaultDefn(intOf($1),$3);  sp-=4;}
          ;
crule     : context IMPLIES btype2      {$$ = gc3(pair($1,checkPred($3)));}
          | btype2                      {$$ = gc1(pair(NIL,checkPred($1)));}
          ;
irule     : context IMPLIES btype2      {$$ = gc3(pair($1,checkPred($3)));}
          | btype2                      {$$ = gc1(pair(NIL,checkPred($1)));}
          ;
dtypes    : /* empty */                 {$$ = gc0(NIL);}
          | dtypes1                     {$$ = gc1(rev($1));}
          ;
dtypes1   : dtypes1 ',' type            {$$ = gc3(cons($3,$1));}
          | type                        {$$ = gc1(cons($1,NIL));}
          ;

/*- Type expressions: -----------------------------------------------------*/

sigType   : context IMPLIES type        {$$ = gc3(ap(QUAL,pair($1,$3)));}
          | type                        {$$ = gc1($1);}
          ;
topType   : context IMPLIES topType1    {$$ = gc3(ap(QUAL,pair($1,$3)));}
          | topType1                    {$$ = gc1($1);}
          ;
topType1  : bpolyType ARROW topType1    {$$ = gc3(fn($1,$3));}
          | btype1    ARROW topType1    {$$ = gc3(fn($1,$3));}
          | btype2    ARROW topType1    {$$ = gc3(fn($1,$3));}
          | btype                       {$$ = gc1($1);}
          ;
polyType  : ALL varid1s '.' sigType     {$$ = gc4(ap(POLYTYPE,
                                                     pair(rev($2),$4)));}
          | bpolyType                   {$$ = gc1($1);}
          ;
bpolyType : '(' polyType ')'            {$$ = gc3($2);}
          ;
varid1s   : varid1s ',' varid1          {$$ = gc3(cons($3,$1));}
          | varid1                      {$$ = gc1(cons($1,NIL));}
          ;
context   : '(' ')'                     {$$ = gc2(NIL);}
          | btype2                      {$$ = gc1(singleton(checkPred($1)));}
          | '(' btype2 ')'              {$$ = gc3(singleton(checkPred($2)));}
          | '(' btypes2 ')'             {$$ = gc3(checkContext($2));}
/*#if TREX*/
          | lacks                       {$$ = gc1(singleton($1));}
          | '(' lacks1 ')'              {$$ = gc3(checkContext($2));}
          ;
lacks     : varid1 '\\' varid1          {
#if TREX
                                         $$ = gc3(ap(mkExt(textOf($3)),$1));
#else
                                         noTREX("a type context");
#endif
                                        }
          ;
lacks1    : btypes2 ',' lacks           {$$ = gc3(cons($3,$1));}
          | lacks1  ',' btype2          {$$ = gc3(cons($3,$1));}
          | lacks1  ',' lacks           {$$ = gc3(cons($3,$1));}
          | btype2  ',' lacks           {$$ = gc3(cons($3,cons($1,NIL)));}
          | lacks                       {$$ = gc1(singleton($1));}
          ;
/*#endif*/

type      : type1                       {$$ = gc1($1);}
          | btype2                      {$$ = gc1($1);}
          ;
type1     : btype1                      {$$ = gc1($1);}
          | btype1 ARROW type           {$$ = gc3(fn($1,$3));}
          | btype2 ARROW type           {$$ = gc3(fn($1,$3));}
          | error                       {syntaxError("type expression");}
          ;
btype     : btype1                      {$$ = gc1($1);}
          | btype2                      {$$ = gc1($1);}
          ;
btype1    : btype1 atype                {$$ = gc2(ap($1,$2));}
          | atype1                      {$$ = gc1($1);}
          ;
btype2    : btype2 atype                {$$ = gc2(ap($1,$2));}
          | qconid                      {$$ = gc1($1);}
          ;
atype     : atype1                      {$$ = gc1($1);}
          | qconid                      {$$ = gc1($1);}
          ;
atype1    : varid1                      {$$ = gc1($1);}
          | '(' ')'                     {$$ = gc2(conPreludeUnit);}
          | '(' ARROW ')'               {$$ = gc3(typeArrow);}
          | '(' type1 ')'               {$$ = gc3($2);}
          | '(' btype2 ')'              {$$ = gc3($2);}
          | '(' tupCommas ')'           {$$ = gc3($2);}
          | '(' btypes2 ')'             {$$ = gc3(buildTuple($2));}
          | '(' typeTuple ')'           {$$ = gc3(buildTuple($2));}
/*#if TREX*/
          | '(' tfields ')'             {
#if TREX
                                         $$ = gc3(revOnto($2,typeNoRow));
#else
                                         noTREX("a type");
#endif
                                        }
          | '(' tfields '|' type ')'    {$$ = gc5(revOnto($2,$4));}
/*#endif*/
          | '[' type ']'                {$$ = gc3(ap(conPreludeList,$2));}
          | '[' ']'                     {$$ = gc2(conPreludeList);}
          | '_'                         {$$ = gc1(inventVar());}
          ;
tupCommas : tupCommas ','               {$$ = gc2(mkTuple(tupleOf($1)+1));}
          | ','                         {$$ = gc1(mkTuple(2));}
          ;
btypes2   : btypes2 ',' btype2          {$$ = gc3(cons($3,$1));}
          | btype2  ',' btype2          {$$ = gc3(cons($3,cons($1,NIL)));}
          ;
typeTuple : type1     ',' type          {$$ = gc3(cons($3,cons($1,NIL)));}
          | btype2    ',' type1         {$$ = gc3(cons($3,cons($1,NIL)));}
          | btypes2   ',' type1         {$$ = gc3(cons($3,$1));}
          | typeTuple ',' type          {$$ = gc3(cons($3,$1));}
          ;
/*#if TREX*/
tfields   : tfields ',' tfield          {$$ = gc3(cons($3,$1));}
          | tfield                      {$$ = gc1(singleton($1));}
          ;
tfield    : varid COCO type             {$$ = gc3(ap(mkExt(textOf($1)),$3));}
          ;
/*#endif*/

/*- Value declarations: ---------------------------------------------------*/

decllist  : '{' decls end               {$$ = gc3($2);}
          ;
decls     : /* empty */                 {$$ = gc0(NIL);}
          | ';'                         {$$ = gc1(NIL);}
          | decls1                      {$$ = gc1($1);}
          | decls1 ';'                  {$$ = gc2($1);}
          ;
decls1    : decls1 ';' decl             {$$ = gc3(cons($3,$1));}
          | decl                        {$$ = gc1(cons($1,NIL));}
          ;
/* Sneakily using qvars to eliminate a conflict... */
decl      : qvars COCO topType          {$$ = gc3(sigdecl($2,$1,$3));}
          | opExp rhs                   {$$ = gc2(pair($1,$2));}
          ;
rhs       : rhs1 wherePart              {$$ = gc2(letrec($2,$1));}
          | error                       {syntaxError("declaration");}
          ;
rhs1      : '=' exp                     {$$ = gc2(pair($1,$2));}
          | gdefs                       {$$ = gc1(grded(rev($1)));}
          ;
wherePart : WHERE decllist              {$$ = gc2($2);}
          | /*empty*/                   {$$ = gc0(NIL);}
          ;
gdefs     : gdefs gdef                  {$$ = gc2(cons($2,$1));}
          | gdef                        {$$ = gc1(cons($1,NIL));}
          ;
gdef      : '|' exp '=' exp             {$$ = gc4(pair($3,pair($2,$4)));}
          ;
vars      : vars ',' var                {$$ = gc3(cons($3,$1));}
          | var                         {$$ = gc1(cons($1,NIL));}
          ;
qvars     : qvars ',' qvar              {$$ = gc3(cons($3,$1));}
          | qvar                        {$$ = gc1(cons($1,NIL));}
          ;



var       : varid                       {$$ = gc1($1);}
          | '(' '-' ')'                 {$$ = gc3(varMinus);}
          ;
varid     : varid1                      {$$ = gc1($1);}
          | '(' VAROP ')'               {$$ = gc3($2);}
          | '(' '!' ')'                 {$$ = gc3(varBang);}
          | '(' '.' ')'                 {$$ = gc3(varDot);}
          ;
varid1    : VARID                       {$$ = gc1($1);}
          | HIDING                      {$$ = gc1(varHiding);}
          | QUALIFIED                   {$$ = gc1(varQualified);}
          | ASMOD                       {$$ = gc1(varAsMod);}
          ;
qvar      : qvarid                      {$$ = gc1($1);}
          | '(' qvarsym ')'             {$$ = gc3($2);}
          | '(' '.' ')'                 {$$ = gc3(varDot);}
          | '(' '!' ')'                 {$$ = gc3(varBang);}
          | '(' '-' ')'                 {$$ = gc3(varMinus);}
          ;
qvarid    : varid1                      {$$ = gc1($1);}
          | QVARID                      {$$ = gc1($1);}
          ;

op        : varop                       {$$ = gc1($1);}
          | conop                       {$$ = gc1($1);}
          | '-'                         {$$ = gc1(varMinus);}
          ;
qop       : qvarop                      {$$ = gc1($1);}
          | qconop                      {$$ = gc1($1);}
          | '-'                         {$$ = gc1(varMinus);}
          ;

varop     : VAROP                       {$$ = gc1($1);}
          | '!'                         {$$ = gc1(varBang);}
          | '.'                         {$$ = gc1(varDot);}
          | '`' varid1 '`'              {$$ = gc3($2);}
          ;
qvarop    : qvarsym                     {$$ = gc1($1);}
          | '!'                         {$$ = gc1(varBang);}
          | '.'                         {$$ = gc1(varDot);}
          | '`' qvarid '`'              {$$ = gc3($2);}
          ;
qvarsym   : VAROP                       {$$ = gc1($1);}
          | QVAROP                      {$$ = gc1($1);}
          ;

con       : CONID                       {$$ = gc1($1);}
          | '(' CONOP ')'               {$$ = gc3($2);}
          ;
qcon      : qconid                      {$$ = gc1($1);}
          | '(' qconsym ')'             {$$ = gc3($2);}
          ;
qconid    : CONID                       {$$ = gc1($1);}
          | QCONID                      {$$ = gc1($1);}
          ;
qconsym   : CONOP                       {$$ = gc1($1);}
          | QCONOP                      {$$ = gc1($1);}
          ;

conop     : CONOP                       {$$ = gc1($1);}
          | '`' CONID '`'               {$$ = gc3($2);}
          ;
qconop    : qconsym                     {$$ = gc1($1);}
          | '`' qconid '`'              {$$ = gc3($2);}
          ;

/*- Expressions: ----------------------------------------------------------*/

exp       : exp1                        {$$ = gc1($1);}
          | error                       {syntaxError("expression");}
          ;
exp1      : opExp COCO sigType          {$$ = gc3(ap(ESIGN,pair($1,$3)));}
          | opExp                       {$$ = gc1($1);}
          ;
opExp     : opExp0                      {$$ = gc1(tidyInfix($1));}
          | pfxExp                      {$$ = gc1($1);}
          ;
opExp0    : opExp0 qop '-' pfxExp       {$$ = gc4(ap(NEG,ap(ap($2,$1),$4)));}
          | opExp0 qop pfxExp           {$$ = gc3(ap2($2,$1,$3));}
          | '-' pfxExp                  {$$ = gc2(ap(NEG,only($2)));}
          | pfxExp qop pfxExp           {$$ = gc3(ap(ap($2,only($1)),$3));}
          | pfxExp qop '-' pfxExp       {$$ = gc4(ap(NEG,
                                                     ap(ap($2,only($1)),$4)));}
          ;
pfxExp    : '\\' pats ARROW exp         {$$ = gc4(ap(LAMBDA,      
                                                     pair(rev($2),
                                                          pair($3,$4))));}
          | LET decllist IN exp         {$$ = gc4(letrec($2,$4));}
          | IF exp THEN exp ELSE exp    {$$ = gc6(ap(COND,triple($2,$4,$6)));}
          | CASEXP exp OF '{' alts end  {$$ = gc6(ap(CASE,pair($2,rev($5))));}
          | DO '{' stmts end            {$$ = gc4(ap(DOCOMP,checkDo($3)));}
          | appExp                      {$$ = gc1($1);}
          ;
pats      : pats atomic                 {$$ = gc2(cons($2,$1));}
          | atomic                      {$$ = gc1(cons($1,NIL));}
          ;
appExp    : appExp atomic               {$$ = gc2(ap($1,$2));}
          | atomic                      {$$ = gc1($1);}
          ;
atomic    : qvar                        {$$ = gc1($1);}
          | qvar '@' atomic             {$$ = gc3(ap(ASPAT,pair($1,$3)));}
          | '~' atomic                  {$$ = gc2(ap(LAZYPAT,$2));}
          | '_'                         {$$ = gc1(WILDCARD);}
          | qcon                        {$$ = gc1($1);}
          | qcon '{' fbinds '}'         {$$ = gc4(ap(CONFLDS,pair($1,$3)));}
          | atomic '{' fbinds '}'       {$$ = gc4(ap(UPDFLDS,
                                                     triple($1,NIL,$3)));}
          | '(' ')'                     {$$ = gc2(conPreludeUnit);}
          | NUMLIT                      {$$ = gc1($1);}
          | CHARLIT                     {$$ = gc1($1);}
          | STRINGLIT                   {$$ = gc1($1);}
          | REPEAT                      {$$ = gc1($1);}
          | '(' exp ')'                 {$$ = gc3($2);}
          | '(' exps2 ')'               {$$ = gc3(buildTuple($2));}
/*#if TREX*/
          | '(' vfields ')'             {
#if TREX
                                         $$ = gc3(revOnto($2,nameNoRec));
#else
                                         $$ = gc3(NIL);
#endif
                                        }
          | '(' vfields '|' exp ')'     {$$ = gc5(revOnto($2,$4));}
          | RECSELID                    {$$ = gc1($1);}
/*#endif*/
          | '[' list ']'                {$$ = gc3($2);}
          | '(' pfxExp qop ')'          {$$ = gc4(ap($3,$2));}
          | '(' qvarop atomic ')'       {$$ = gc4(ap2(varFlip,$2,$3));}
          | '(' qconop atomic ')'       {$$ = gc4(ap2(varFlip,$2,$3));}
          | '(' tupCommas ')'           {$$ = gc3($2);}
          ;
exps2     : exps2 ',' exp               {$$ = gc3(cons($3,$1));}
          | exp ',' exp                 {$$ = gc3(cons($3,cons($1,NIL)));}
          ;
/*#if TREX*/
vfields   : vfields ',' vfield          {$$ = gc3(cons($3,$1));}
          | vfield                      {$$ = gc1(singleton($1));}
          ;
vfield    : qvarid '=' exp              {
#if TREX
                                         $$ = gc3(ap(mkExt(textOf($1)),$3));
#else
                                         noTREX("an expression");
#endif
                                        }
          ;
/*#endif*/
alts      : alts1                       {$$ = gc1($1);}
          | alts1 ';'                   {$$ = gc2($1);}
          ;
alts1     : alts1 ';' alt               {$$ = gc3(cons($3,$1));}
          | alt                         {$$ = gc1(cons($1,NIL));}
          ;
alt       : opExp altRhs wherePart      {$$ = gc3(pair($1,letrec($3,$2)));}
          ;
altRhs    : guardAlts                   {$$ = gc1(grded(rev($1)));}
          | ARROW exp                   {$$ = gc2(pair($1,$2));}
          | error                       {syntaxError("case expression");}
          ;
guardAlts : guardAlts guardAlt          {$$ = gc2(cons($2,$1));}
          | guardAlt                    {$$ = gc1(cons($1,NIL));}
          ;
guardAlt  : '|' opExp ARROW exp         {$$ = gc4(pair($3,pair($2,$4)));}
          ;
stmts     : stmts1 ';'                  {$$ = gc2($1);}
          | stmts1                      {$$ = gc1($1);}
          ;
stmts1    : stmts1 ';' stmt             {$$ = gc3(cons($3,$1));}
          | stmt                        {$$ = gc1(cons($1,NIL));}
          ;
stmt      : exp1 FROM exp               {$$ = gc3(ap(FROMQUAL,pair($1,$3)));}
          | LET decllist                {$$ = gc2(ap(QWHERE,$2));}
          | IF exp                      {$$ = gc2(ap(BOOLQUAL,$2));}
          | exp1                        {$$ = gc1(ap(DOQUAL,$1));}
          ;
fbinds    : /* empty */                 {$$ = gc0(NIL);}
          | fbinds1                     {$$ = gc1(rev($1));}
          ;
fbinds1   : fbinds1 ',' fbind           {$$ = gc3(cons($3,$1));}
          | fbind                       {$$ = gc1(singleton($1));}
          ;
fbind     : var                         {$$ = gc1($1);}
          | qvar '=' exp                {$$ = gc3(pair($1,$3));}
          ;

/*- List Expressions: -------------------------------------------------------*/

list      : /* empty */                 {$$ = gc0(conPreludeNil);}
          | exp                         {$$ = gc1(ap(FINLIST,cons($1,NIL)));}
          | exps2                       {$$ = gc1(ap(FINLIST,rev($1)));}
          | exp '|' quals               {$$ = gc3(ap(COMP,pair($1,rev($3))));}
          | exp         UPTO exp        {$$ = gc3(ap2(varEnumFromTo,$1,$3));}
          | exp ',' exp UPTO            {$$ = gc4(ap2(varEnumFromThen,$1,$3));}
          | exp         UPTO            {$$ = gc2(ap1(varEnumFrom,$1));}
          | exp ',' exp UPTO exp        {$$ = gc5(ap3(varEnumFromThenTo,
                                                      $1,$3,$5));}
          ;
quals     : quals ',' qual              {$$ = gc3(cons($3,$1));}
          | qual                        {$$ = gc1(cons($1,NIL));}
          ;
qual      : exp FROM exp                {$$ = gc3(ap(FROMQUAL,pair($1,$3)));}
          | exp                         {$$ = gc1(ap(BOOLQUAL,$1));}
          | LET decllist                {$$ = gc2(ap(QWHERE,$2));}
          ;

/*- Tricks to force insertion of leading and closing braces ---------------*/

begin     : error                       {yyerrok; goOffside(startColumn);}
          ;
                                        /* deal with trailing semicolon    */
end       : '}'                         {$$ = gc1($1);}
          | error                       {yyerrok; 
                                         if (canUnOffside()) {
                                             unOffside();
                                             /* insert extra token on stack*/
                                             push(NIL);
                                             pushed(0) = pushed(1);
                                             pushed(1) = mkInt(column);
                                         }
                                         else
                                             syntaxError("definition");
                                        }
          ;

/*-------------------------------------------------------------------------*/

%%

static Cell local gcShadow(n,e)         /* keep parsed fragments on stack  */
Int  n;
Cell e; {
    /* If a look ahead token is held then the required stack transformation
     * is:
     *   pushed: n               1     0          1     0
     *           x1  |  ...  |  xn  |  la   ===>  e  |  la
     *                                top()            top()
     *
     * Othwerwise, the transformation is:
     *   pushed: n-1             0        0
     *           x1  |  ...  |  xn  ===>  e
     *                         top()     top()
     */
    if (yychar>=0) {
        pushed(n-1) = top();
        pushed(n)   = e;
    }
    else
        pushed(n-1) = e;
    sp -= (n-1);
    return e;
}

static Void local syntaxError(s)       /* report on syntax error           */
String s; {
    ERRMSG(row) "Syntax error in %s (unexpected %s)", s, unexpected()
    EEND;
}

static String local unexpected() {     /* find name for unexpected token   */
    static char buffer[100];
    static char *fmt = "%s \"%s\"";
    static char *kwd = "keyword";

    switch (yychar) {
        case 0         : return "end of input";

#define keyword(kw) sprintf(buffer,fmt,kwd,kw); return buffer;
        case INFIXL    : keyword("infixl");
        case INFIXR    : keyword("infixr");
        case INFIX     : keyword("infix");
        case FOREIGN   : keyword("foreign");
        case UNSAFE    : keyword("unsafe");
        case TINSTANCE : keyword("instance");
        case TCLASS    : keyword("class");
        case CASEXP    : keyword("case");
        case OF        : keyword("of");
        case IF        : keyword("if");
        case THEN      : keyword("then");
        case ELSE      : keyword("else");
        case WHERE     : keyword("where");
        case TYPE      : keyword("type");
        case DATA      : keyword("data");
        case TNEWTYPE  : keyword("newtype");
        case LET       : keyword("let");
        case IN        : keyword("in");
        case DERIVING  : keyword("deriving");
        case DEFAULT   : keyword("default");
        case IMPORT    : keyword("import");
        case EXPORT    : keyword("export");
        case MODULETOK : keyword("module");
        case INTERFACE : keyword("interface");
        case WILDCARD  : keyword("_");
        case ALL       : keyword("forall");
#undef keyword

        case ARROW     : return "`->'";
        case '='       : return "`='";
        case COCO      : return "`::'";
        case '-'       : return "`-'";
        case '!'       : return "`!'";
        case ','       : return "comma";
        case '@'       : return "`@'";
        case '('       : return "`('";
        case ')'       : return "`)'";
        case '{'       : return "`{'";
        case '}'       : return "`}'";
        case '_'       : return "`_'";
        case '|'       : return "`|'";
        case '.'       : return "`.'";
        case ';'       : return "`;'";
        case UPTO      : return "`..'";
        case '['       : return "`['";
        case ']'       : return "`]'";
        case FROM      : return "`<-'";
        case '\\'      : return "backslash (lambda)";
        case '~'       : return "tilde";
        case '`'       : return "backquote";
#if TREX
        case RECSELID  : sprintf(buffer,"selector \"#%s\"",
                                 textToStr(extText(snd(yylval))));
                         return buffer;
#endif
        case VAROP     :
        case VARID     :
        case CONOP     :
        case CONID     : sprintf(buffer,"symbol \"%s\"",
                                 textToStr(textOf(yylval)));
                         return buffer;
        case QVAROP    :
        case QVARID    :
        case QCONOP    : 
        case QCONID    : sprintf(buffer,"symbol \"%s\"",
                                 identToStr(yylval));
                         return buffer;
        case HIDING    : return "symbol \"hiding\"";
        case QUALIFIED : return "symbol \"qualified\"";
        case ASMOD     : return "symbol \"as\"";
        case NUMLIT    : return "numeric literal";
        case CHARLIT   : return "character literal";
        case STRINGLIT : return "string literal";
        case IMPLIES   : return "`=>'";
        default        : return "token";
    }
}

static Cell local checkPrec(p)         /* Check for valid precedence value */
Cell p; {
    if ((!isInt(p) || intOf(p)<MIN_PREC || intOf(p)>MAX_PREC)
        && (!isBignum(p) || bignumOf(p)<MIN_PREC || bignumOf(p)>MAX_PREC)
        ) {
        ERRMSG(row) "Precedence value must be an integer in the range [%d..%d]",
                    MIN_PREC, MAX_PREC
        EEND;
    }
    if (isBignum(p)) {
        return mkInt(bignumOf(p));
    } else {
        return p;
    }
}

static Void local fixDefn(a,line,p,ops)/* Declare syntax of operators      */
Syntax a;
Cell   line;
Cell   p;
List   ops; {
    Int l = intOf(line);
    a     = mkSyntax(a,intOf(p));
    map2Proc(setSyntax,l,a,ops);
}

static Void local setSyntax(line,sy,op)/* set syntax of individ. operator  */
Int    line;
Syntax sy;
Cell   op; {
    addSyntax(line,textOf(op),sy);
    opDefns = cons(op,opDefns);
}

static Cell local buildTuple(tup)      /* build tuple (x1,...,xn) from list*/
List tup; {                            /* [xn,...,x1]                      */
    Int  n = 0;
    Cell t = tup;
    Cell x;

    do {                               /*     .                    .       */
        x      = fst(t);               /*    / \                  / \      */
        fst(t) = snd(t);               /*   xn  .                .   xn    */
        snd(t) = x;                    /*        .    ===>      .          */
        x      = t;                    /*         .            .           */
        t      = fun(x);               /*          .          .            */
        n++;                           /*         / \        / \           */
    } while (nonNull(t));              /*        x1  NIL   (n)  x1         */
    fst(x) = mkTuple(n);
    return tup;
}

static List local checkContext(con)     /* validate context                */
Type con; {
    mapOver(checkPred, con);
    return con;
}

static Cell local checkPred(c)          /* check that type expr is a valid */
Cell c; {                               /* constraint                      */
    Cell cn = getHead(c);
#if TREX
    if (isExt(cn) && argCount==1)
        return c;
#endif
    if (!isQCon(cn) || argCount==0)
        syntaxError("class expression");
    return c;
}

static Pair local checkDo(dqs)          /* convert reversed list of dquals */
List dqs; {                             /* to an (expr,quals) pair         */
    if (isNull(dqs) || whatIs(hd(dqs))!=DOQUAL) {
        ERRMSG(row) "Last generator in do {...} must be an expression"
        EEND;
    }
    fst(dqs) = snd(fst(dqs));           /* put expression in fst of pair   */
    snd(dqs) = rev(snd(dqs));           /* & reversed list of quals in snd */
    return dqs;
}

static Cell local checkTyLhs(c)         /* check that lhs is of the form   */
Cell c; {                               /* T a1 ... a                      */
    Cell tlhs = c;
    while (isAp(tlhs) && whatIs(arg(tlhs))==VARIDCELL)
        tlhs = fun(tlhs);
    switch (whatIs(tlhs)) {
        case CONIDCELL  : return c;

        default :
            ERRMSG(row) "Illegal left hand side in datatype definition"
            EEND;
    }
}

#if !TREX
static Void local noTREX(where)
String where; {
    ERRMSG(row) "Attempt to use Typed Records with Extensions\nwhile parsing %s. This feature is disabled in this build of Hugs.",
                 where
    EEND;
}
#endif

/* Expressions involving infix operators or unary minus are parsed as elements
 * of the following type:
 *
 *     data OpExp = Only Exp | Neg OpExp | Infix OpExp Op Exp
 *
 * (The algorithms here do not assume that negation can be applied only once,
 * i.e., that - - x is a syntax error, as required by the Haskell report.
 * Instead, that restriction is captured by the grammar itself, given above.)
 *
 * There are rules of precedence and grouping, expressed by two functions:
 *
 *     prec :: Op -> Int;   assoc :: Op -> Assoc    (Assoc = {L, N, R})
 *
 * OpExp values are rearranged accordingly when a complete expression has
 * been read using a simple shift-reduce parser whose result may be taken
 * to be a value of the following type:
 *
 *     data Exp = Atom Int | Negate Exp | Apply Op Exp Exp | Error String
 *
 * The machine on which this parser is based can be defined as follows:
 *
 *     tidy                         :: OpExp -> [(Op,Exp)] -> Exp
 *     tidy (Only a)      []         = a
 *     tidy (Only a)      ((o,b):ss) = tidy (Only (Apply o a b)) ss
 *     tidy (Infix a o b) []         = tidy a [(o,b)]
 *     tidy (Infix a o b) ((p,c):ss)
 *                      | shift  o p = tidy a ((o,b):(p,c):ss)
 *                      | red    o p = tidy (Infix a o (Apply p b c)) ss
 *                      | ambig  o p = Error "ambiguous use of operators"
 *     tidy (Neg e)       []         = tidy (tidyNeg e) []
 *     tidy (Neg e)       ((o,b):ss)
 *                      | nshift o   = tidy (Neg (underNeg o b e)) ss
 *                      | nred   o   = tidy (tidyNeg e) ((o,b):ss)
 *                      | nambig o   = Error "illegal use of negation"
 *
 * At each stage, the parser can either shift, reduce, accept, or error.
 * The transitions when dealing with juxtaposed operators o and p are
 * determined by the following rules:
 *
 *     shift o p  = (prec o > prec p)
 *               || (prec o == prec p && assoc o == L && assoc p == L)
 *
 *     red o p    = (prec o < prec p)
 *               || (prec o == prec p && assoc o == R && assoc p == R)
 *
 *     ambig o p  = (prec o == prec p)
 *               && (assoc o == N || assoc p == N || assoc o /= assoc p)
 *
 * The transitions when dealing with juxtaposed unary minus and infix operators
 * are as follows.  The precedence of unary minus (infixl 6) is hardwired in
 * to these definitions, as it is to the definitions of the Haskell grammar
 * in the official report.
 *
 *     nshift o   = (prec o > 6)
 *     nred   o   = (prec o < 6) || (prec o == 6 && assoc o == L)
 *     nambig o   = prec o == 6 && (assoc o == R || assoc o == N)
 *
 * An OpExp of the form (Neg e) means negate the last thing in the OpExp e;
 * we can force this negation using:
 *
 *     tidyNeg              :: OpExp -> OpExp
 *     tidyNeg (Only e)      = Only (Negate e)
 *     tidyNeg (Infix a o b) = Infix a o (Negate b)
 *     tidyNeg (Neg e)       = tidyNeg (tidyNeg e)
 * 
 * On the other hand, if we want to sneak application of an infix operator
 * under a negation, then we use:
 *
 *     underNeg                  :: Op -> Exp -> OpExp -> OpExp
 *     underNeg o b (Only e)      = Only (Apply o e b)
 *     underNeg o b (Neg e)       = Neg (underNeg o b e)
 *     underNeg o b (Infix e p f) = Infix e p (Apply o f b)
 *
 * As a concession to efficiency, we lower the number of calls to syntaxOf
 * by keeping track of the values of sye, sys throughout the process.  The
 * value APPLIC is used to indicate that the syntax value is unknown.
 */

#define UMINUS_PREC  6                  /* Change these settings at your   */
#define UMINUS_ASSOC LEFT_ASS           /* own risk; they may not work!    */

static Cell local tidyInfix(e)          /* convert OpExp to Expr           */
Cell e; {                               /* :: OpExp                        */
    Cell s     = NIL;                   /* :: [(Op,Exp)]                   */
    Syntax sye = APPLIC;                /* Syntax of op in e (init unknown)*/
    Syntax sys = APPLIC;                /* Syntax of op in s (init unknown)*/

    for (;;)
        switch (whatIs(e)) {
            case ONLY : e = snd(e);
                        while (nonNull(s)) {
                            Cell next   = arg(fun(s));
                            arg(fun(s)) = e;
                            e           = s;
                            s           = next;
                        }
                        return e;

            case NEG  : if (nonNull(s)) {

                            if (sys==APPLIC) {  /* calculate sys           */
                                sys = identSyntax(fun(fun(s)));
                                if (sys==APPLIC) sys=DEF_OPSYNTAX;
                            }

                            if (precOf(sys)==UMINUS_PREC &&     /* nambig  */
                                assocOf(sys)!=UMINUS_ASSOC) {
                                ERRMSG(row)
                                 "Ambiguous use of unary minus with \"%s\"",
                                   textToStr(textOf(fun(fun(s))))
                                EEND;
                            }

                            if (precOf(sys)>UMINUS_PREC) {      /* nshift  */
                                Cell e1    = snd(e);
                                Cell t     = s;
                                s          = arg(fun(s));
                                while (whatIs(e1)==NEG)
                                    e1 = snd(e1);
                                arg(fun(t)) = arg(e1);
                                arg(e1)     = t;
                                sys         = APPLIC;
                                continue;
                            }
                        
                        }

                        /* Intentional fall-thru for nreduce and isNull(s) */
                        {   Cell prev = e;              /* e := tidyNeg e  */
                            Cell temp = arg(prev);
                            Int  nneg = 1;
                            for (; whatIs(temp)==NEG; nneg++) {
                                fun(prev) = varNegate;
                                prev      = temp;
                                temp      = arg(prev);
                            }
                            /* These special cases are required for
                             * pattern matching.
                             */
                            if (isInt(arg(temp))) {     /* special cases   */
                                if (nneg&1)             /* for literals    */
                                    arg(temp) = intNegate(arg(temp));
                            }
                            else if (isBignum(arg(temp))) {
                                if (nneg&1) 
                                    arg(temp) = bignumNegate(arg(temp));
                            }
                            else if (isFloat(arg(temp))) {
                                if (nneg&1) 
                                    arg(temp) = floatNegate(arg(temp));
                            }
                            else {
                                fun(prev) = varNegate;
                                arg(prev) = arg(temp);
                                arg(temp) = e;
                            }
                            e = temp;
                        }
                        continue;

            default   : if (isNull(s)) {/* Move operation onto empty stack */
                            Cell next   = arg(fun(e));
                            s           = e;
                            arg(fun(s)) = NIL;
                            e           = next;
                            sys         = sye;
                            sye         = APPLIC;
                        }
                        else {          /* deal with pair of operators     */

                            if (sye==APPLIC) {  /* calculate sys and sye   */
                                sye = identSyntax(fun(fun(e)));
                                if (sye==APPLIC) sye=DEF_OPSYNTAX;
                            }
                            if (sys==APPLIC) {
                                sys = identSyntax(fun(fun(s)));
                                if (sys==APPLIC) sys=DEF_OPSYNTAX;
                            }

                            if (precOf(sye)==precOf(sys) &&     /* ambig   */
                                (assocOf(sye)!=assocOf(sys) ||
                                 assocOf(sye)==NON_ASS)) {
                                ERRMSG(row)
                                "Ambiguous use of operator \"%s\" with \"%s\"",
                                  textToStr(textOf(fun(fun(e)))),
                                  textToStr(textOf(fun(fun(s))))
                                EEND;
                            }

                            if (precOf(sye)>precOf(sys) ||      /* shift   */
                                (precOf(sye)==precOf(sys) &&
                                 assocOf(sye)==LEFT_ASS &&
                                 assocOf(sys)==LEFT_ASS)) {
                                Cell next   = arg(fun(e));
                                arg(fun(e)) = s;
                                s           = e;
                                e           = next;
                                sys         = sye;
                                sye         = APPLIC;
                            }
                            else {                              /* reduce  */
                                Cell next   = arg(fun(s));
                                arg(fun(s)) = arg(e);
                                arg(e)      = s;
                                s           = next;
                                sys         = APPLIC;
                                /* sye unchanged */
                            }
                        }
                        continue;
        }
}

/*-------------------------------------------------------------------------*/
