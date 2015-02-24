/**
 *  Derived from the Haskell Language Specification
 *
 *  https://www.haskell.org/onlinereport/haskell2010/haskellch10.html#x17-17500010
 *
 *  author: Ali Afroozeh
 */

module haskell::specification::Haskell

extend haskell::specification::Lexical;

syntax Module	
     = "module" ModId Exports? "where" Body 
     | Body
     ;

syntax Body	
     = "{" ImpDecls ";" TopDecls "}"
 	 | "{" ImpDecls "}"
	 | "{" TopDecls "}"
	 | "{" "}"
     ; 

syntax ImpDecls	
     = ImpDecl (";" ImpDecl?)*
     ;
 
syntax Exports	
     = "(" {Export ","}* ","? ")"
     ;
 
syntax Export	
     = QVar
	 | ConId ( ("(" ".." ")") | ("(" { (QVar | Con) "," }* ")") )? 
	 | "module" ModId
	 ;
 
syntax ImpDecl	
     = "import" "qualified"? ModId ("as" ModId)? ImpSpec?
	 ;
 
syntax ImpSpec	
     = "(" {Import ","}* ","? ")"
	 | "hiding" "(" { Import ","}* ","? ")"
	 ;
 
syntax Import	
     = Var
	 | ConId ( ("(" ".." ")") | ("(" { (QVar | Con) "," }* ")") )?
	 ;

syntax CName	
     = Var 
     | Con
     ;
 
syntax TopDecls	
     = TopDecl ( ";" TopDecl? )*
     ;

syntax TopDecl	
     = "data" (Context "=\>")? SimpleType ("=" Constrs)? Deriving?
	 | "data" (Context "=\>")? SimpleType "where" GADTDecls      			// Generalized Abstarct Data Types extension
	 | "newtype" (Context "=\>")? SimpleType "=" NewConstr Deriving?
	 | "class" (SContext "=\>")? TyCls TyVar+ ("where" CDecls)?
	 | "instance" (Context "=\>")? Context ("where" CDecls)?            // Flexible instances
	 | "deriving" "instance" (Context "=\>")? QTyCls Inst 				// Extension
	 | "default" {Type ","}*
	 | "foreign" FDecl
	 | Decl
	 ;
 
syntax Decls	
     = "{" { Decl? ";"}+ "}"
     ;

syntax Decl	
     = GenDecl
	 | (FunLHS | Pat) RHS
	 | AssociatedTypeDecl
	 ;
 
syntax CDecls	
     = "{" {CDecl? ";"}+ "}"
     ;

syntax CDecl	
     = GenDecl
     | (FunLHS | Var) RHS
     | AssociatedTypeDecl
	 ;
 
syntax GenDecl	
     = Vars "::" CType	    
	 | Fixity Integer? Ops
	 ;
	 
syntax GADTDecls
     = "{" { GADTDecl ";" }+ "}"
     ;
     
syntax GADTDecl
     = TyCon "::" CType
     | TyCon "::" "{" {(Var ("::" CType)?) ","}+ "}" "-\>" CType
     ;
     
     
// Associated type declarations
syntax AssociatedTypeDecl
     = "type" "family"? Type ("::" Kind)?
     | "type" "instance"? TypeFamilyInstEqn
     ;
     
syntax TypeFamilyInstEqn
     = Type "=" CType
     ;
     
syntax CType
     = "forall" TVBinder* "." CType
     | Context "=\>" CType
     | Type
     ;     
     
syntax TVBinder
     = TyVar                  
	 | "(" TyVar "::" Kind ")"
	 ;     
	 
syntax Kind
     = AKind* ("-\>" Kind)?
     ;

syntax AKind
     = "*"
     | "(" Kind ")"
     | PKind
     | TyVar
     ;

syntax PKind
     = QTyCon
     | "(" ")"
     | "(" Kind "," { Kind "," }+ ")"
     | "[" Kind "]"
     ;
     
 
syntax Ops	
     = { Op "," }+
     ;

syntax Vars
     = { Var ","}+
     ;

syntax Fixity	
     = "infixl"
     | "infixr" 
     | "infix"
     ;
 
syntax Type	
     = BType 
     | BType "-\>" CType 
     | BType "~" BType   
     ;
 
syntax BType	
     = BType? "!"? AType	    
     ;
 
syntax AType	
     = GTyCon
	 | TyVar
	 | "(" "#"? { CType "," }+ "#"? ")"  // GHC Extension: unboxed tuples	    
	 | "[" CType "]"	    				    
	 ;
	 
syntax GTyCon	
     = QTyCon
	 | "(" ")"	    		
	 | "[" "]"	    		
	 | "(" "-\>" ")"	     
 	 | "(" ","+ ")"	     
 	 ;
 
syntax Context	
     = Class
	 | "(" {Class ","}* ")"	
	 ;

syntax Class	
     = QTyCls AType* ("~" Class)? // To deal with flexible contexts and type equality
	 ;
	 
syntax SContext	
     = SimpleClass
	 | "(" { SimpleClass "," }* ")"
	 ;

syntax SimpleClass	
     = QTyCls TyVar
     ;
 
syntax SimpleType	
     = TyCon TyVar*
     ;

syntax Constrs	
     = { Constr "|" }+
     ;

syntax Constr	
     = Con ("!"? AType)*                                   
	 | BType ConOp BType	    
	 | ("forall" TVBinder* ".")? Con "{" { FieldDecl ","}* "}"
	 ;

syntax NewConstr	
     = Con AType
	 | Con "{" Var "::" CType "}"
	 ;

syntax FieldDecl	
     = Vars "::" CType
     ;

syntax Deriving	
     = "deriving" (DClass | ("(" { DClass "," }* ")") )	    
     ;

syntax DClass	
     = QTyCls
     ;
 
syntax Inst	
     = GTyCon
	 | "(" GTyCon TyVar* ")"	              
	 | "(" TyVar "," { TyVar "," }+ ")"	    
	 | "[" TyVar "]"
	 | "(" TyVar "-\>" TyVar ")"	             
	 ;
 
syntax FDecl	
     = "import" CallConv Safety? Impent Var "::" FType	    
	 | "export" CallConv Expent Var "::" FType	         
	 ;

syntax CallConv
     = "ccall" 
     | "stdcall" 
     | "cplusplus"
	 | "jvm" 
     | "dotnet"
//	| system-specific calling conventions
	 ;

syntax Impent	
     = String?	    
     ;

syntax Expent
     = String?	    
     ;

syntax Safety	
     = "unsafe" 
     | "safe"
     ;
 
syntax FType	
     = FRType
	 | FAType  "-\>"  FType
	 ;

syntax FRType	
     = FAType
	 | "("")"
	 ;

syntax FAType	
     = QTyCon AType*
     ;
 
syntax FunLHS
     = Var APat+
	 | Pat VarOp Pat
	 | "(" FunLHS ")" APat+
	 ;
 
syntax RHS	
     = "=" Exp ("where" Decls)?
	 | GDRHS ("where" Decls)?
	 ;
 
syntax GDRHS
     = Guards "=" Exp GDRHS?
     ;
 
syntax Guards	
     = "|" { Guard "," }+
     ;

syntax Guard	
     = Pat "\<-" InfixExp	    
	 | "let" Decls	         
	 | InfixExp	         
	 ;
 
syntax Exp	
     = InfixExp "::" CType	    
	 | InfixExp
	 ;
 
syntax InfixExp	
     = LExp1 QOp InfixExp
	 | "-" InfixExp	         
	 | LExp
	 ;
 
syntax LExp	
     = "\\" APat+ "-\>" InfixExp
	 | "let" Decls "in" Exp
	 | "if" Exp ";"? "then" Exp ";"? "else" Exp
	 | "case" Exp "of" "{" Alts "}"   
	 | "do" "{" Stmts "}"                 
	 | FExp
	 ;
	 
syntax LExp1	
     = "case" Exp "of" "{" Alts "}"   
	 | "do" "{" Stmts "}"                 
	 | FExp
	 ;
	 
syntax FExp	
     = FExp? AExp	    
     ;
 
syntax AExp	
     = QVar	                             
	 | GCon !>> "."							   // To disambiguate with "." in QualifiedNames        
	 | Literal
	 | "(" "#"? { Exp "," }+ "#"? ")"		   // GHC Extension: Unboxed tuples
	 | "[" { Exp ","}+ "]"
	 | "[" Exp ("," Exp)? ".." Exp? "]"	    
	 | "[" Exp "|" { Qual "," }+ "]"
	 | "(" InfixExp QOp ")"
	 | "(" QOp \ "-" InfixExp ")" 
	 | AExp "{" { FBind "," }* "}"	    
	 ;
 
syntax Qual	
     = Pat "\<-" Exp
	 | "let" Decls
	 | Exp	    
	 ;
 
syntax Alts	
     = { Alt ";" }+
     ;

syntax Alt
     = Pat "-\>" Exp ("where" Decls)?
	| Pat GDPat ("where" Decls)?
	|
	;
 
syntax GDPat	
     = Guards "-\>" Exp GDPat?
     ;
 
syntax Stmts	
     = Stmt* Exp ";"?
     ;

syntax Stmt
     = Exp ";"
	 | Pat "\<-" Exp ";"
	 | "let" Decls ";"
	 | ";"
	 ;
 
syntax FBind	
     = QVar ("=" Exp)?
     ;
 
syntax Pat	
     = LPat QConOp Pat
	 | LPat
	 ;
 
syntax LPat	
     = APat
	 | "-" (Integer | Float)
	 | GCon APat+
	 ;
 
syntax APat
     = Var ( "@" APat)?
     | "!" APat						 // GHC Extension: Bang patterns
	 | GCon
	 | QCon "{" { FPat "," }* "}"
	 | Literal
	 | "_" !>> [A-Za-z_\-]
	 | "(" "#"? {Pat ","}+ "#"? ")"  // GHC Extension: unboxed types
	 | "[" { Pat "," }+ "]"
	 | "~" APat
	 | "(" Var "::" CType ")"        // Scoped Type Variables
	 ;
 
syntax FPat	
     = QVar ("=" Pat)?
     | ".."					// GHC Extension RecordWildCards
     ;
 
syntax GCon
     = "(" ")"
	 | "[" "]"
	 | "(" ","+ ")"
	 | QCon
	 ;
 
syntax Var	
     = VarId
     | "(" VarSym ")"
     ;     

syntax QVar	
     = QVarId 
     | "(" QVarSym ")"
     ;

syntax Con	
     = ConId 
     | "(" ConSym ")"
     ;

syntax QCon	
     = QConId 
     | "(" GConSym ")"
     ;

syntax VarOp
     = VarSym 
     | "`"  VarId "`"
     ;

syntax QVarOp	
     = QVarSym 
     | "`" QVarId "`"
     ;

syntax ConOp	
     = ConSym 
     | "`" ConId "`"
     ;

syntax QConOp
     = GConSym 
     | "`" QConId "`"
     ;

syntax Op	
     = VarOp 
     | ConOp
     ;

syntax QOp	
     = QVarOp 
     | QConOp
     ;

syntax GConSym	
     = ":" 
     | QConSym
     ;
