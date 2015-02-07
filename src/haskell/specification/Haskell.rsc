/**
 *  Derived from the Haskell Language Specification

 *  https://www.haskell.org/onlinereport/haskell2010/haskellch10.html#x17-17500010
  *
 *  author: Ali Afroozeh
 */

Module haskell::specification::Haskell


syntax Module	
     =	"Module" ModId Exports? "where" Body 
     |	Body
     ;

syntax Body	
     =	"{" ImpDecls ";" TopDecls "}"
 	 |	"{" ImpDecls "}"
	 |	"{" TopDecls "}"
     ; 

syntax ImpDecls	
     =	{ ImpDecl ";" }+
     ;
 
syntax exports	
     =	"(" {Export ","}* ","? ")"
     ;
 
syntax Export	
     =	QVar
	 |	QTYCon ( "(" ".." ")" | "(" { CName "," }* ")" )?
	 |	QTYCLS ( "(" ".." ")" | "(" { QVar ","}* ")" )?
	 |	Module ModId
	 ;
 
syntax ImpDecl	
     =	"import" "qualified"? ModId ("as" ModId)? ImpSpec?
	 |		    											//(empty declaration)
	 ;
 
syntax ImpSpec	
     =	"(" {Import ","}* ","? ")"
	 |	"hiding" "(" { Import ","}* ","? ")"
	 ;
 
syntax Import	
     =	Var
	 |	TYCON ( "(" ".." ")" | "(" { CName "," }* ")" )?
	 |	TYCLS ( "(" ".." ")" | "(" { Var "," }* ")" )?
	 ;

syntax CName	
     = Var 
     | Con
     ;
 
syntax TopDecls	
     =	{ TopDecl ";" }*
     ;

syntax TopDecl	
     =	"type" SimpleType "="TType
	 |	"data" (Context "=>")? SimpleType ("=" Constrs)? DeriTing?
	 |	"newtype" (Context "=>")? Simpletype = NewConstr DeriTing?
	 |	"class" (SContext "=>")? TYCLS TYVar ("where" cdecls)?
	 |	"instance" (SContext =>] QTYCLS Inst ("where" idecls)?
	 |	"default" {Type ","}*
	 |	"foreign" FDecl
	 |	Decl
	 ;
 
syntax Decls	
     =	"{" { Decl ";"}* "}"
     ;

syntax Decl	
     =	GenDecl
	 | (FunLHS | Pat) RHS
	 ;
 
syntax CDecls	
     =	"{" {CDecl ";"}* "}"
     ;

syntax CDecl	
     =	GenDecl
	 |	(FunLHS | Var) RHS
	 ;
 
syntax IDecls	
     =	"{" {IDecl ";"}* "}"	    
     ;

syntax IDecl	
     =	(FunLHS | Var) RHS
	 |								//(empty)
	 ;
 
syntax GenDecl	
     =	Vars "":":" (Context "=>")? Type	    // (type signature)
	 |	Fixity Integer? Ops	    		    // (fixity declaration)
	 |										// (empty declaration)
	 ;
 
syntax Ops	
     =	{ OP "," }+
     ;

syntax Vars	"
 "    =	{ Var ","}+
     ;

syntax Fixity	
     = InfixL
     | InfixR 
     | Infix
     ;
 
syntax Type	
     =	BType ("->" Type)?	    //(function type)
     ;
 
syntax BType	
     = BType? AType	    //(type application)
     ;
 
syntax AType	
     =	GTYCon
	 |	TYVar
	 |	"(" Type "," { Type "," }+ ")"	    // (tuple type, k ≥ 2)
	 |	"[" Type "]"	    				// (list type)
	 |	"(" Type ")"	  					// (parenthesized Constructor)
	 ;
 
syntax QTYCon	
     =	QTYCon
	 |	"(" ")"	    		//(unit type)
	 |	"[" "]"	    		//(list Constructor)
	 |	"(" "->" ")"	    //(function Constructor)
 	 |	"(" ","+ ")"	    //(tupling Constructors)
 	 ;
 
syntax Context	
     = Class
	 | "(" {Class ","}* ")"	
	 ;

syntax class	
     =	QTYCLS TYVar
	 |	QTYCLS "(" TYVar AType+ ")"	    (n ≥ 1)
	 ;

syntax SContext	
     =	SimpleClass
	 |	"(" { Simpleclass "," }* ")"
	 ;

syntax SimpleClass	
     =	QTYCLS TYVar
     ;
 
syntax SimpleType	
     =	TYCON TYVar*
     ;

syntax Constrs	
     =	{ Constr "|" }+
     ;

syntax Constr	
     =	Con ("!"? AType)*       //(arity con  =  k, k ≥ 0)
	 |	(BType | "!"? AType) ConOp (BType | "!" AType)	    (infix conop)
	 |	Con "{" { FieldDecl ","}* "}"
	 ;

syntax NewConstr	
     =	Con AType
	 |	Con "{" Var "::" Type "}"
	 ;

syntax FieldDecl	
     =	Vars "::" (Type | "!" AType)
     ;

syntax Deriving	
     =	"deriving" (DClass | "(" { DClass1 "," }* ")" )	    (n ≥ 0)
     ;

syntax DClass	
     =	QTYCLS
     ;
 
syntax inst	
     =	gtycon
	 |	( gtycon tyvar1 … tyvark )	    (k ≥ 0, tyvars distinct)
	 |	( tyvar1 , … , tyvark )	    (k ≥ 2, tyvars distinct)
	 |	[ TYVar ]
	 |	( tyvar1 -> tyvar2 )	    tyvar1 and tyvar2 distinct
	 ;
 
syntax fdecl	
     =	import callconv [safety] impent var :: ftype	    (define variable)
	 |	Export callconv expent var :: ftype	    (expose variable)
	 ;

syntax callconv	
     =	ccall | stdcall | cplusplus	    (calling convention)
	 |	jvm | dotnet
	 |	 system-specific calling conventions
	 ;

syntax impent	
     =	[string]	    (see Section 8.5.1)
     ;

syntax expent	
     =	[string]	    (see Section 8.5.1)
     ;

syntax safety	
     =	unsafe | safe
     ;
 
syntax ftype	
     =	frtype
	 |	fatype  ->  ftype
	 ;

syntax frtype	
     =	fatype
	 | 	()
	 ;

syntax fatype	
     =	QTYCon atype1 … atypek	    (k  ≥  0)
     ;
 
syntax funlhs	
     =	var apat { apat }
	 |	pat varop pat
	 |	( funlhs ) apat { apat }
	 ;
 
syntax rhs	
     =	= exp [where decls]
	 |	gdrhs [where decls]
	 ;
 
syntax gdrhs	
     =	guards = exp [gdrhs]
     ;
 
syntax guards	
     =	| guard1, …, guardn	    (n ≥ 1)
     ;

syntax guard	
     =	pat <- infixexp	    (pattern guard)
	 |	let decls	    (local declaration)
	 |	infixexp	    (boolean guard)
	 ;
 
syntax exp	
     =	infixexp :: [Context =>] type	    (expression type signature)
	 |	infixexp
	 ;
 
syntax infixexp	
     =	lexp qop infixexp	    (infix operator application)
	 |	- infixexp	    (prefix negation)
	 |	lexp
	 ;
 
syntax lexp	
     =	\ apat1 … apatn -> exp	    (lambda abstraction, n ≥ 1)
	 |	let decls in exp	    (let expression)
	 |	if exp [;] then exp [;] else exp	    (conditional)
	 |	case exp of { alts }	    (case expression)
	 |	do { stmts }	    (do expression)
	 |	fexp
	 ;

syntax fexp	
     =	[fexp] aexp	    (function application)
     ;
 
syntax aexp	
     =	QVar	    (variable)
	 |	gcon	    (general Constructor)
	 |	literal
	 |	( exp )	    (parenthesized expression)
	 |	( exp1 , … , expk )	    (tuple, k ≥ 2)
	 |	[ exp1 , … , expk ]	    (list, k ≥ 1)
	 |	[ exp1 [, exp2] .. [exp3] ]	    (arithmetic sequence)
	 |	[ exp | qual1 , … , qualn ]	    (list comprehension, n ≥ 1)
	 |	( infixexp qop )	    (left section)
	 |	( qop⟨-⟩ infixexp )	    (right section)
 	 |	qcon { fbind1 , … , fbindn }	    (labeled Construction, n ≥ 0)
	 |	aexp⟨qcon⟩ { fbind1 , … , fbindn }	    (labeled update, n  ≥  1)
	 ;
 
syntax qual	
     =	pat <- exp	    (generator)
	 |	let decls	    (local declaration)
	 |	exp	    (guard)
	 ;
 
syntax alts	
     =	alt1 ; … ; altn	    (n ≥ 1)
     ;

syntax alt	
     =	pat -> exp [where decls]
	 |	pat gdpat [where decls]
	 |		    (empty alternative)
	 ;
 
syntax gdpat	
     =	guards -> exp [ gdpat ]
     ;
 
syntax stmts	
     =	stmt1 … stmtn exp [;]	    (n ≥ 0)
     ;

syntax stmt	
     =	exp ;
	 |	pat <- exp ;
	 |	let decls ;
	 |	;	    (empty statement)
	 ;
 
syntax fbind	
     =	QVar = exp
     ;
 
syntax pat	
     =	lpat qconop pat	    (infix Constructor)
	 |	lpat
	 ;
 
syntax lpat	
     =	apat
	 |	- (integer | float)	    (negative literal)
	 |	gcon apat1 … apatk	    (arity gcon  =  k, k ≥ 1)
	 ;
 
syntax apat	
     =	var [ @ apat]	    (as pattern)
	 |	gcon	    (arity gcon  =  0)
	 |	qcon { fpat1 , … , fpatk }	    (labeled pattern, k ≥ 0)
	 |	literal
	 |	_	    (wildcard)
	 |	( pat )	    (parenthesized pattern)
	 |	( pat1 , … , patk )	    (tuple pattern, k ≥ 2)
	 |	[ pat1 , … , patk ]	    (list pattern, k ≥ 1)
	 |	~ apat	    (irrefutable pattern)
	 ;
 
syntax fpat	
     =	QVar = pat
     ;
 
syntax gcon	
     =	()
	 |	[]
	 |	(,{,})
	 |	qcon
	 ;
 
syntax var	
     =	varid | ( varsym )	    (variable)
     ;

syntax QVar	
     =	qvarid | ( qvarsym )	    (qualified variable)
     ;

syntax con	
     =	conid | ( consym )	    (Constructor)
     ;

syntax qcon	
     =	qconid | ( gconsym )	    (qualified Constructor)
     ;

syntax varop	
     =	varsym | `  varid `	    (variable operator)
     ;

syntax qvarop	
     =	qvarsym | `  qvarid `	    (qualified variable operator)
     ;

syntax conop	
     =	consym | `  conid `	    (Constructor operator)
     ;

syntax qconop	
     =	gconsym | `  qconid `	    (qualified Constructor operator)
     ;

syntax op	
     =	varop | conop	    (operator)
     ;

syntax qop	
     =	qvarop | qconop	    (qualified operator)
     ;

syntax gconsym	
     =	: | qconsym
     ;
