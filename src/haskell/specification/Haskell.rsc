/**
 *  Derived from the Haskell Language Specification
 
 *  https://www.haskell.org/onlinereport/haskell2010/haskellch10.html#x17-17500010
  *
 *  author: Ali Afroozeh
 */

module java::specification::Java


syntax module	
     =	module modid [exports] where body 
     |	body
     ;

syntax body	
     =	{ impdecls ; topdecls }
 	 |	{ impdecls }
	 |	{ topdecls }
     ; 

syntax impdecls	
     =	impdecl1 ; … ; impdecln	    (n ≥ 1)
     ;
 
syntax exports	
     =	( export1 , … , exportn [ , ] )	    (n ≥ 0)
     ;
 
syntax export	
     =	qvar
	 |	qtycon [(..) | ( cname1 , … , cnamen )]	    (n ≥ 0)
	 |	qtycls [(..) | ( qvar1 , … , qvarn )]	    (n ≥ 0)
	 |	module modid
	 ;
 
syntax impdecl	
     =	import [qualified] modid [as modid] [impspec]
	 |		    (empty declaration)
	 ;
 
syntax impspec	
     =	( import1 , … , importn [ , ] )	    (n ≥ 0)
	 |	hiding ( import1 , … , importn [ , ] )	    (n ≥ 0)
	 ;
 
syntax import	
     =	var
	 |	tycon [ (..) | ( cname1 , … , cnamen )]	    (n ≥ 0)
	 |	tycls [(..) | ( var1 , … , varn )]	    (n ≥ 0)

syntax cname	
     =	var 
     | con
     ;
 
syntax topdecls	
     =	topdecl1 ; … ; topdecln	    (n ≥ 0)
     ;

syntax topdecl	
     =	type simpletype = type
	 |	data [context =>] simpletype [= constrs] [deriving]
	 |	newtype [context =>] simpletype = newconstr [deriving]
	 |	class [scontext =>] tycls tyvar [where cdecls]
	 |	instance [scontext =>] qtycls inst [where idecls]
	 |	default (type1 , … , typen)	    (n ≥ 0)
	 |	foreign fdecl
	 |	decl
	 ;
 
syntax decls	
     =	{ decl1 ; … ; decln }	    (n ≥ 0)
     ;

syntax decl	
     =	gendecl
	 |	(funlhs | pat) rhs
	 ;
 
syntax cdecls	
     =	{ cdecl1 ; … ; cdecln }	    (n ≥ 0)
     ;

syntax cdecl	
     =	gendecl
	 |	(funlhs | var) rhs
	 ;
 
syntax idecls	
     =	{ idecl1 ; … ; idecln }	    (n ≥ 0)
     ;

syntax idecl	
     =	(funlhs | var) rhs
	 |	(empty)
	 ;
 
syntax gendecl	
     =	vars :: [context =>] type	    (type signature)
	 |	fixity [integer] ops	    (fixity declaration)
	 |  (empty declaration)
	 ;
 
syntax ops	
     =	op1 , … , opn	    (n ≥ 1)
     ;

syntax vars	
     =	var1 , …, varn	    (n ≥ 1)
     ;

syntax fixity	
     =	infixl 
     | infixr 
     | infix
     ;
 
syntax type	
     =	btype [-> type]	    (function type)
     ;
 
syntax btype	
     =	[btype] atype	    (type application)
     ;
 
syntax atype	
     =	gtycon
	 |	tyvar
	 |	( type1 , … , typek )	    (tuple type, k ≥ 2)
	 |	[ type ]	    (list type)
	 |	( type )	    (parenthesized constructor)
	 ;
 
syntax gtycon	
     =	qtycon
	 |	()	    (unit type)
	 |	[]	    (list constructor)
	 |	(->)	    (function constructor)
 	 |	(,{,})	    (tupling constructors)
 	 ;
 
syntax context	
     =	class
	 |	( class1 , … , classn )	    (n ≥ 0)
	 ;

syntax class	
     =	qtycls tyvar
	 |	qtycls ( tyvar atype1 … atypen )	    (n ≥ 1)
	 ;

syntax scontext	
     =	simpleclass
	 |	( simpleclass1 , … , simpleclassn )	    (n ≥ 0)
	 ;

syntax simpleclass	
     =	qtycls tyvar
     ;
 
syntax simpletype	
     =	tycon tyvar1 … tyvark	    (k ≥ 0)
     ;

syntax constrs	
     =	constr1 | … | constrn	    (n ≥ 1)
     ;

syntax constr	
     =	con [!] atype1 … [!] atypek	    (arity con  =  k, k ≥ 0)
	 |	(btype | ! atype) conop (btype | ! atype)	    (infix conop)
	 |	con { fielddecl1 , … , fielddecln }	    (n ≥ 0)
	 ;

syntax newconstr	
     =	con atype
	 |	con { var :: type }
	 ;

syntax fielddecl	
     =	vars :: (type | ! atype)
     ;

syntax deriving	
     =	deriving (dclass | (dclass1, … , dclassn))	    (n ≥ 0)
     ;

syntax dclass	
     =	qtycls
     ;
 
syntax inst	
     =	gtycon
	 |	( gtycon tyvar1 … tyvark )	    (k ≥ 0, tyvars distinct)
	 |	( tyvar1 , … , tyvark )	    (k ≥ 2, tyvars distinct)
	 |	[ tyvar ]
	 |	( tyvar1 -> tyvar2 )	    tyvar1 and tyvar2 distinct
	 ;
 
syntax fdecl	
     =	import callconv [safety] impent var :: ftype	    (define variable)
	 |	export callconv expent var :: ftype	    (expose variable)
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
     =	qtycon atype1 … atypek	    (k  ≥  0)
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
     =	infixexp :: [context =>] type	    (expression type signature)
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
     =	qvar	    (variable)
	 |	gcon	    (general constructor)
	 |	literal
	 |	( exp )	    (parenthesized expression)
	 |	( exp1 , … , expk )	    (tuple, k ≥ 2)
	 |	[ exp1 , … , expk ]	    (list, k ≥ 1)
	 |	[ exp1 [, exp2] .. [exp3] ]	    (arithmetic sequence)
	 |	[ exp | qual1 , … , qualn ]	    (list comprehension, n ≥ 1)
	 |	( infixexp qop )	    (left section)
	 |	( qop⟨-⟩ infixexp )	    (right section)
 	 |	qcon { fbind1 , … , fbindn }	    (labeled construction, n ≥ 0)
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
     =	qvar = exp
     ;
 
syntax pat	
     =	lpat qconop pat	    (infix constructor)
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
     =	qvar = pat
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

syntax qvar	
     =	qvarid | ( qvarsym )	    (qualified variable)
     ;

syntax con	
     =	conid | ( consym )	    (constructor)
     ;

syntax qcon	
     =	qconid | ( gconsym )	    (qualified constructor)
     ;

syntax varop	
     =	varsym | `  varid `	    (variable operator)
     ;

syntax qvarop	
     =	qvarsym | `  qvarid `	    (qualified variable operator)
     ;

syntax conop	
     =	consym | `  conid `	    (constructor operator)
     ;

syntax qconop	
     =	gconsym | `  qconid `	    (qualified constructor operator)
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
