module OCaml

// Top-level     		
start syntax Interface = specifications: (Specification ";;"?)*; 
    	 
start syntax Implementation = definitions: (Definition ";;"?)*;

start syntax TopLevel = toplevels: TopLevelPhrase*
			          | toplevels1: TopLevelPhrase* Expr
			          | toplevels2: TopLevelPhrase* Definition
			          ;

syntax TopLevelPhrase 
	 = topLevelDefinitions: Definition+ ";;"
   	 | expr: Expr ";;"
   	 ;     		
 
// Names
syntax ValuePath 
	 = valuePath: (ModulePath ".")? ValueName;

syntax ValueName 
	= LowercaseIdentifier 
	| "(" OperatorChar+ !>> [! : \< = \> ? @ ^ | ~] ")"   // This is added to cover cases such as let (!) x y = x + y
	| "(" ("mod"| "lsl" | "lsr" | "asr" | "mod" | "land" | "lor" | "lxor") ")" 
	;   

syntax TagName = Ident;

syntax TypeconstrName = LowercaseIdentifier;

syntax TypeConstr = typeConstr: (ExtendedModulePath ".")? TypeconstrName;

syntax ConstrName = CapitalizedIdentifier;

syntax LabelName = LowercaseIdentifier;

syntax ModuleName = CapitalizedIdentifier;

syntax FieldName = LowercaseIdentifier;

syntax ClassName = LowercaseIdentifier;

syntax InstVarName = LowercaseIdentifier;

syntax MethodName = LowercaseIdentifier;

syntax ModTypeName = Ident;

syntax ModulePath = modulePath: (ModuleName ".")* ModuleName;

syntax Constr = const: (ModulePath ".")? ConstrName !>> (Dot ());

lexical Dot = [\ \r\n\t]*[.][\ \r\n\t]*[A-Z];

syntax Field = field_name: (ModulePath ".")? FieldName;

syntax ClassPath = classPath: (ModulePath "." )? ClassName;

syntax ModTypePath = modTypePath: (ExtendedModulePath "." )? ModTypName;

syntax ModTypName = Ident;

syntax ExtendedModulePath = extendedModulePath1: (ExtendedModulePath ".")? ModuleName
                          | extendedModulePath2: ExtendedModulePath "(" ExtendedModulePath ")";


// Type expressions

syntax Typexpr 
	 = typexprConstr1: Typexpr TypeConstr
	 > non-assoc star: Typexpr "*" {Typexpr !star "*"}+
	 > right (arrow1: Typexpr "-\>" Typexpr
	 |        arrow2: "?"? LabelName ":" Typexpr !arrow1 "-\>" Typexpr)
	 > typexprAsId: Typexpr "as" "\'" Ident 
	 > typexprPrivate: "private" Typexpr
	 | tagg: "\'" Ident
     | anyTypexpr: "_" !>> [a-zA-Z0-9]
     | typeExprBrackets: "(" Typexpr ")"
     | typexprConstr2: TypeConstr
  	 | typeExprBrackets2: "(" Typexpr ("," Typexpr)+ ")" TypeConstr
  	 | polymorphicVariantType: PolymorphicVariantType
  	 | typexprEmptyAngleBrackets: "\<" ".."? "\>"
  	 | typexprAngleBrackets: "\<" {MethodType ";"}+ (";" "..")? "\>"
  	 | typexprHash1: "#" ClassPath
  	 | typexprHash2: Typexpr "#" ClassPath
  	 | typexprHash3: "(" {Typexpr ","}+ ")" "#" ClassPath
  	 | typexprPackage: "(" "module" PackageType ")"  
     ;

    
syntax PolymorphicVariantType
 	 = polymorphicVariantType1: "[" "|"? {TagSpec "|"}* "]"
     | polymorphicVariantType2: "[\>" {TagSpec "|"}* "]"
     | polymorphicVariantType3: "[\<"  "|"? {TagSpecFull "|"}+ ("\>" ("`" TagName)+ )?  "]"
     ;
       
syntax PolyTypExpr 
	 = polytype1: Typexpr
     | polytype2: ("\'" Ident)+ "." Typexpr
     ;
       
syntax MethodType 
	= methodType: MethodName ":" PolyTypExpr;
       
       
syntax TagSpec
     = tagSpec1: "`" TagName ("of" Typexpr)?
	 | tagSpec2: Typexpr
	 ;

syntax TagSpecFull 
	 = tagSpecFull1: "`" TagName ("of" Typexpr)? ("&" Typexpr)*
     | tagSpecFull2: Typexpr
     ;


// Expressions

syntax Expr 
	 = prefix: 				PrefixSymbol Expr !valuePath
	 > non-assoc field: 			Expr "." Field  
	 | non-assoc dotBracket1: 		Expr ".(" Expr ")"
	 | non-assoc dotBracket2: 		Expr ".[" Expr "]"
	 | non-assoc dotBracket3: 		Expr ".{" Expr "}"
	 > hash: 				Expr "#" MethodName
     > non-assoc 
     (
     functionApplication: 	Expr  Arg+
     //| constrExp: 			Constr Expr    To avoid ambiguities with Expr Arg+ as Expr can derive Constr
     //| polyVariant:	 		"`" TagName Expr  To Avoid ambiguities with Constant("`" TagName) Arg(Expr)
     | lazy: 				"lazy" Expr
     | assertExpr: 			"assert" Expr
     )
     > unaryMinus: 			"-"  Expr | floatUnaryMinus: "-." Expr
     > right infix1: 		Expr InfixSymbol1 Expr
     > left  infix2: 		Expr InfixSymbol2 Expr
     > left  infix3: 		Expr InfixSymbol3 Expr   // to disambiguate [|   5.2026032092;     19132e-10;  -39e-10 |];
     > right coloncolon:	Expr "::" Expr
     > right infix4: 		Expr InfixSymbol4 Expr
     > left  infix5: 		Expr InfixSymbol5 Expr
     | left  uneq:   		Expr "!=" Expr
     > right infix6: 		Expr InfixSymbol6 Expr
     > right infix7: 		Expr InfixSymbol7 Expr
     > non-assoc comma: 	Expr ("," Expr !comma !sep)+
     > right 
     (
       assign1: 			Expr "." Field "\<-" Expr
     | assign2:		 		Expr ".(" Expr ")" "\<-" Expr
     | assign3: 	 		Expr ".[" Expr "]" "\<-" Expr
     | assign4: 	 		Expr ".{" Expr "}" "\<-" Expr
     | assign5:		 		InstVarName "\<-" Expr
     )
     > right infix8: 		Expr InfixSymbol8 Expr
     > ifThenElse: 	 		"if" Expr "then" Expr "else" Expr
     | ifThen: 		 		"if"  Expr "then" Expr !>> (Else ())
     //> semicolon: 	 		Expr ";" !>>  ";"
     > right sep: 	 		Expr ";" Expr
     > match: 		 		"match" Expr "with" PatternMatching
     | function: 	 		"function" PatternMatching
     | fun: 		 		"fun" MultipleMatching
     | tryBlock: 	 		"try" Expr "with" PatternMatching     
     | letbinding: 	 		"let" "rec"? {LetBinding "and"}+ "in" Expr
     | letModule:	 		"let" "module" ModuleName "="  ModuleExpr "in"  Expr 
     | brackets: 			"(" Expr ")"
  	 | beginEnd: 	 		"begin" Expr "end"
  	 | brackets1: 	 		"(" Expr ":" Typexpr ")"
  	 | brackets2:	 		"(" Expr ":\>"  Typexpr ")"  
 	 | brackets3: 	 		"(" Expr ":"  Typexpr ":\>"  Typexpr ")"  
 	 | brackets4: 	 		"{\<" InstVarName "=" Expr !sep  (";" InstVarName "="  Expr)*  ";"? "\>}"  
  	 | tupl: 		 		"["  {Expr !sep ";"}+ ";"? "]"
     | array: 		 		"[|" {Expr !sep ";"}+ ";"? "|]"
     | record1:	     		"{" Field ("=" Expr !sep)? (";" Field ("=" Expr !sep)?)* ";"? "}"
     | record2: 	 		"{" Expr "with" Field ("=" Expr !sep )? (";" Field ("=" Expr !sep)?)* ";"? "}"
     | whileloop: 	 		"while" Expr "do" Expr "done"
     | forloop: 			"for" Ident "=" Expr ("to" | "downto") Expr "do" Expr "done"
     | new: 				"new" ClassPath
     | object: 		 		"object" ClassBody "end"  
     | moduleExpr: 	 		"(" "module" ModuleExpr  (":" PackageType)? ")"  
     //| valuePath: 	 		ValuePath
     | 						ValueName
	 | constant: 			Constant
	 //| 						InstVarName
     ; 
     
     
lexical Else = [\ \r \n \t]* "else";     
     
syntax Arg 
 	 =                Expr !functionApplication !constrExp !polyVariant !lazy !assertExpr !unaryMinus !floatUnaryMinus !infix1 !infix2 !infix3 
 	                       !coloncolon !infix4 !infix5 !uneq !infix6 !infix7 !comma !assign1 !assign2 !assign3 !assign4 !assign5 
 	                       !infix8 !ifThenElse !ifThen !sep !match !function !fun !tryBlock !letbinding !letModule 
 	 | label: 		  Label
     | labelColon:    LabelColon Expr !functionApplication !polyVariant !lazy !assertExpr !unaryMinus !floatUnaryMinus !infix1 !infix2 !infix3 
 	                       			  !coloncolon !infix4 !infix5 !uneq !infix6 !infix7 !comma !assign1 !assign2 !assign3 !assign4 !assign5 
 	                       			  !infix8 !ifThenElse !ifThen !sep !match !function !fun !tryBlock !letbinding !letModule
     | optlabel:      OptLabel
     | optlabelColon: OptLabelColon Expr !functionApplication !polyVariant !lazy !assertExpr !unaryMinus !floatUnaryMinus !infix1 !infix2 !infix3 
 	                       				 !coloncolon !infix4 !infix5 !uneq !infix6 !infix7 !comma !assign1 !assign2 !assign3 !assign4 !assign5 
 	                       				 !infix8 !ifThenElse !ifThen !sep !match !function !fun !tryBlock !letbinding !letModule
     ;
           
syntax PatternMatching 
     = patternMatching: "|"? Pattern ("when" Expr)? "-\>" Expr InnerPatternMatching* !>> (Bar ()) 
     ;
     
lexical Bar = [\ \n \r \t]*[|];     
     
syntax InnerPatternMatching
	 = innerPatternMatching: "|" Pattern ("when" Expr)? "-\>" Expr
	 ;     
           
syntax LetBinding 
     =                  Pattern !patternValueName "="  Expr  
	 | letBinding:      ValueName Parameter* (":" PolyTypExpr)? (":\>" Typexpr)? "=" Expr 
	 | bindingNew:		ValueName ":" "type"  TypeConstr* "."  Typexpr "="  Expr
	 ;
	 
syntax MultipleMatching
     = multipleMatching: Parameter+ ("when" Expr)? "-\>" Expr;	 

syntax Parameter 
	 = patternParam: Pattern
     | param1: 		 "~" LabelName !>> ":"
     | param2:		 "~" "(" LabelName (":" Typexpr)? ")"
     | param3:		 "~" LabelName ":" Pattern
     | param4:		 "?" LabelName !>> ":"
     | param5:		 "?" "(" LabelName (":" Typexpr)? ("=" Expr)? ")"
     | param6: 		 "?" LabelName ":" Pattern
     | param7: 		 "?" LabelName ":" "(" Pattern ":" Typexpr "=" Expr ")"
     | typeParam:	 "(" "type" TypeconstrName ")"  
     ;

// Patterns

syntax Pattern 
	 = constrPattern: 		  Constr Pattern
	 > tagNamePattern: 		  "`" TagName Pattern
	 > right listCons: 		  Pattern "::" Pattern
	 > non-assoc patterns: 	  Pattern "," {Pattern !patterns !patternBar !patternAs ","}+
	 > left patternBar: 	  Pattern "|" Pattern
	 > patternAs: 			  Pattern "as" ValueName
	 | patternValueName: 	  ValueName
     | anyPattern: 			  "_" !>> [a-zA-Z0-9]   // To enforce longest match with identifiers
     | patternConstant: 	  Constant
     | patternRange: 		  CharLiteral ".." CharLiteral   // Extensions
     | patternBrackets: 	  "(" Pattern ")"
     | patternTypxprBrackets: "(" Pattern ":" Typexpr ")"
     | patternHash: 		  "#" TypeconstrName
     | patternRec: 			  "{" Field ("=" Pattern)? (";" Field "=" Pattern)* ";"? "}"
     | patternTuple: 		  "["  {Pattern ";"}+ ";"? "]"
     | patternArray: 		  "[|" {Pattern ";"}+ ";"? "|]"
     | lazyPattern: 		  "lazy" Pattern
     | patternPackage: 		  "(" "module" ModuleName  (":" PackageType)? ")"  
     ;       
         
syntax Constant 
     = posInt: 			IntegerLiteral
     | floatLiteral: 	FloatLiteral
     | charLiteral: 	CharLiteral
     | stringLiteral: 	StringLiteral1
     | constr: 			Constr
     | falseConstant: 	"false"
     | trueConstant: 	"true"
     | emptyParenthesis: "(" ")"
	 | emptyBrackets:	"[" "]"
	 | emptyArray: 		"[|" "|]"
	 | emptyCurly: 		"{\<" "\>}"
     | 					"`" TagName
     | int32: 			Int32Literal  
	 | int64: 			Int64Literal  
     | nativeInt: 		NativeIntLiteral
     ;

// ModuleExpressions 

syntax Definition 
	 = defVal: "val" ValueName ":" Typexpr
	 | letDef: "let" "rec"? LetBinding  ("and" LetBinding)*
     | external: "external" ValueName ":" Typexpr "=" ExternalDeclaration
     | typeDef: TypeDefinition
     | exceptionDef: ExceptionDefinition
     | classDef:ClassDefinition
     | classTypeDef: ClassTypeDefinition
     | moduleDef1: "module" ModuleName ( "(" ModuleName ":" ModuleType ")" )* ( ":" ModuleType )? "=" ModuleExpr
     | moduleDef2: "module" ModuleName ("(" ModuleName ":" ModuleType ")")* ":" ModuleType
     | modType1: "module" "type" ModTypeName "=" ModuleType
     | modType2: "module" "type" ModTypeName
     | modRec1: "module" "rec" ModuleName ":"  ModuleType "="  ModuleExpr  ("and" ModuleName ":"  ModuleType "="  ModuleExpr)*
     | modRec2: "module" "rec" ModuleName ":"  ModuleType  ("and" ModuleName ":"  ModuleType)*
     | open: "open" ModulePath
     | include: "include" ModuleExpr
     ;
     
syntax ModuleExpr 
     = modExpModPath: ModulePath
     | struct1: "struct" ((Definition+ ";;") | (Expr ";;"))* "end"
     | struct2: "struct" Definition+ "end"
     | Expr (";;" Expr)* ";;"? (Definition (";;" Expr)* ";;"?)* "end"
     | functor: "functor" "(" ModuleName ":" ModuleType ")" "-\>" ModuleExpr
     | modApp: ModuleExpr "(" ModuleExpr ")"
     | modExprBrackets: "(" ModuleExpr ")"
     | moduleExprType: "(" ModuleExpr ":" ModuleType ")"
     | moduleExprVal: "(" "val" Expr  (":" PackageType)? ")" 
     ;      
     
// ModuleTypes
 	
syntax Specification 
	 = specificationVal: "val" ValueName ":" Typexpr
     | external: 		 "external" ValueName ":" Typexpr "=" ExternalDeclaration
     | typeDef: 		 TypeDefinition
     | exceptionDef:	 ExceptionDefinition
     | classSpec: 		 ClassSpecification
     | classDef: 		 ClassDefinition
     | classTypeDef: 	 ClassTypeDefinition
     | moduleDef1: 		 "module" ModuleName ( "(" ModuleName ":" ModuleType ")" )* ( ":" ModuleType )? "=" ModuleExpr
     | moduleDef2: 		 "module" ModuleName ("(" ModuleName ":" ModuleType ")")* ":" ModuleType
     | modType1: 		 "module" "type" ModTypeName "=" ModuleType
     | modType2: 		 "module" "type" ModTypeName
     | open: 			 "open" ModulePath
     | includeSpec: 	 "include" ModuleType
     ;
     
     
syntax ModuleType 
     = modTypePath:       ModTypePath
     | sig:               "sig" ( Specification ";;"? )* "end"
     | modTypeOf:         "module" "type" "of" ModuleExpr
     | modTypeWith:       ModuleType "with" ModConstraint ("and" ModConstraint)*
     > functor:           "functor" "(" ModuleName ":" ModuleType ")" "-\>" ModuleType
     | bracketModType1:   "(" ModuleType ")"
     | bracketModType2:   ModuleType !modTypeWith "(" ModuleType ")"
     ;


syntax ModConstraint 
	 = modConsType1: "type" TypeParams? TypeConstr "=" Typexpr
	 | modConsType2: "type" TypeParameters?  TypeconstrName ":="  TypeParameters?  TypeConstr
     | modeConsModule1: "module" ModulePath "=" ExtendedModulePath
 	 | modeConsModule2: "module" ModuleName ":="  ExtendedModulePath  
     ; 	


// Type And Exceptions

syntax TypeDefinition 
     = typeDefinition: "type" {TypeDef "and"}+;
     
syntax TypeDef 
     = typeDef: TypeParams? TypeconstrName TypeInformation;

syntax TypeInformation 
     = typeInformation: TypeEquation? TypeRepresentation? TypeConstraint*;

syntax TypeEquation 
     = typeEquation: "=" Typexpr
     ;
          
syntax TypeRepresentation 
 	= constrDecls: "=" "private"? "|"? {ConstrDecl "|"}+
    | fieldDecls: "=" "private"? "{" {FieldDecl ";"}+ ";"? "}"
    ;

syntax TypeParams 
     = singleTypeParam: TypeParam
     | typeParamList: "(" {TypeParam ","}+ ")"
     ;

syntax TypeParam 
     = typeParam1: Variance? "\'" Ident
     | typeParam2: Variance? "_" !>> [a-zA-Z0-9]
     ;     
     
syntax Variance 
     = posVariance: "+" 
     | negVariance: "-";
     
syntax ConstrDecl 
     = constDecl1: ConstrName ("of" { Typexpr !star "*"}+)?
     | constDecl2: ConstrName ":" { Typexpr !star "*" }+ "-\>"  Typexpr
     ;

syntax FieldDecl 
	 = fieldDecl: "mutable"? FieldName ":" PolyTypExpr
	 ;


syntax TypeConstraint 
	 = typeConstraint: "constraint" "\'" Ident "=" Typexpr;
     
syntax ExceptionDefinition 
	 = exception1: "exception" ConstrName ("of" Typexpr !star ("*" Typexpr !star)* )?
     | exception2: "exception" ConstrName "=" Constr
     ;
     
     
// Classes

syntax ClassType 
     = classType: (("?"? LabelName ":")? Typexpr "-\>")* ClassBodyType;

syntax ClassBodyType 
     = classBodyType1: "object" ("(" Typexpr ")")? ClassFieldSpec* "end"
     | classBodyType2: ("[" Typexpr ("," Typexpr)* "]")? ClassPath
     ;

syntax ClassFieldSpec 
     = fieldSpec1: "inherit" ClassType
     | fieldSpec2: "val" "mutable"? "virtual"? InstVarName ":" PolyTypExpr
     | fieldSpec3: "method" "private"? "virtual"? MethodName ":" PolyTypExpr
     | fieldSpec4: "constraint" Typexpr "=" Typexpr
     ;
     
syntax ClassExpr 
     = classPath: ClassPath
	 | classExprBrackets1: "[" Typexpr ("," Typexpr)* "]" ClassPath
	 | classExprBrackets2: "(" ClassExpr ")"
	 | classExprBrackets3: "(" ClassExpr ":" ClassType ")"
	 | classArgs: ClassExpr ! classArgs Arg+
	 | classFun: "fun" Parameter+ "-\>" ClassExpr
	 | letClass: "let" "rec"? LetBinding ("and" LetBinding)* "in" ClassExpr
	 | object: "object" ClassBody "end"
	 ;

syntax ClassField 
     = inheritance:      ("inherit" | "inherit!") ClassExpr ("as" ValueName)?
     | classValue:       ("val"|"val!") "mutable"? InstVarName (":" Typexpr)? "=" Expr
     | virtualValue:     "val" "mutable"? "virtual" InstVarName ":" Typexpr
     |                   ("method" | "method!") "private"? MethodName Parameter* (":" PolyTypExpr)? "=" Expr     
     | method3:          "method" "private"? "virtual" MethodName ":" PolyTypExpr
     | classConstraint:  "constraint" Typexpr "=" Typexpr
     | classInitializer: "initializer" Expr
     ;
     
syntax ClassDefinition 
	 = classDefinition: "class" {ClassBinding "and"}+;     
     
syntax ClassBody 
	 = classBody: ("(" Pattern (":" Typexpr)? ")")? ClassField*;

syntax ClassBinding 
	 = classBinding: "virtual"? ("[" TypeParameters "]")? ClassName Parameter* (":" ClassType)? "=" ClassExpr;

syntax TypeParameters 
	 = typeParameters: "\'" Ident ("," "\'" Ident)*;
	 
syntax ClassSpecification 
	 = classSpecification: "class" ClassSpec ("and" ClassSpec)*;

syntax ClassSpec 
	 = classSpec: "virtual"? ("[" TypeParameters "]")? ClassName ":" ClassType;

syntax ClassTypeDefinition 
	 = classTypeDefinition: "class" "type" ClasstypeDef ("and" ClasstypeDef)*;

syntax ClasstypeDef 
	 = classTypeDef: "virtual"? ("[" TypeParameters "]")? ClassName "=" ClassBodyType;

     
syntax ExternalDeclaration 
     = externalDecl: StringLiteral1+;	 
     


// Extensions

syntax PackageType
	 =	packageType1: ModTypePath  
	 |	packageType2: ModTypePath "with"  PackageConstraint  ("and" PackageConstraint)*
 	 ;  

syntax PackageConstraint
     = packageConstraint: "type" TypeConstr "="  Typexpr
     ;


// Lexical

lexical Ident = LowercaseIdentifier | CapitalizedIdentifier; 

// underscore is considered a lower case identifier
lexical LowercaseIdentifier = ([a-zA-Z_0-9] !<< [a-z_] [A-Za-z0-9_\']* !>> [A-Za-z0-9_\']) \ Keywords;

lexical CapitalizedIdentifier = ([a-zA-Z_0-9] !<< [A-Z] [A-Za-z0-9_\']* !>> [A-Za-z0-9_\']) \ Keywords;

lexical IntegerLiteral1 = [0-9] [0-9_]* !>> [0-9_.eE];

lexical Int32Literal = SpecialInt [l];  
 
lexical Int64Literal = SpecialInt [L];  
 
lexical NativeIntLiteral =	SpecialInt [n];

lexical SpecialInt = [0-9] [0-9_]* !>> [0-9_.eE]
				   | ("0x"| "0X") [0-9A-Fa-f][0-9A-Fa-f_]* !>> [0-9_A-Fa-f.eE]  
				   | ("0o"| "0O") [0-7] [0-7_]* !>> [0-7_.eE]
				   | ("0b"| "0B") [0-1] [0-1_]* !>> [0-1_.eE]
				   ;

lexical IntegerLiteral = [0-9] [0-9_]* !>> [0-9_.eElLn]
					   | ("0x"| "0X") [0-9A-Fa-f][0-9A-Fa-f_]* !>> [0-9_A-Fa-f.eElLn]  
 					   | ("0o"| "0O") [0-7] [0-7_]* !>> [0-7_.eElLn]
 					   | ("0b"| "0B") [0-1] [0-1_]* !>> [0-1_.eElLn]
 					   ;

lexical FloatLiteral =  [0-9] [0-9_]* [eE] [+\-]? [0-9] [0-9_]* !>> [0-9_.eE\-]             // only with e
				     |  [0-9] [0-9_]* [.] [0-9_]* !>> [0-9_.eE\-]                           // only with .
                     |  [0-9] [0-9_]* [.] [0-9_]* [eE] [+\-]? [0-9] [0-9_]* !>> [0-9_.eE\-] // with both . and e
					 ;
					 
lexical CharLiteral = [\'] (RegularChar | EscapeSequence) [\'];
                            
lexical EscapeSequence = ([\\] [\\ \" \' n t b r])
	                   | ([\\] [0-9][0-9][0-9])
	                   | ([\\][x] [0-9A-Fa-f][0-9A-Fa-f]);
                            
lexical StringLiteral1 = [\"] StringCharacter* [\"];

lexical StringCharacter = RegularCharStr |  EscapeSequence | [\\][\n] | [\\][\ ];

lexical RegularChar = ![\'\\];

lexical RegularCharStr = ![\"\\];

lexical OperatorChar = [! $ % & * + \- . / : \< = \> ? @ ^ | ~];

lexical PrefixSymbol = ([!] OperatorChar*) !>> [! $ % & * + \- . / : \< = \> ? @ ^ | ~] \ "!="
   	                 | [? ~] OperatorChar+  !>> [! $ % & * + \- . / : \< = \> ? @ ^ | ~]
	                 ;

lexical Label =	"~" LowercaseIdentifier !>> ":";                 
lexical LabelColon =	"~" LowercaseIdentifier ":";
lexical OptLabel = "?" LowercaseIdentifier !>> ":";
lexical OptLabelColon = "?" LowercaseIdentifier ":";	                 

lexical InfixSymbol1 = "lsl" | "lsr" | "asr" | ([*][*] OperatorChar* !>> [! $ % & * + \- . / : \< = \> ? @ ^ | ~]) \ InfixSymbol1Exclude;
lexical InfixSymbol2 = "mod" | "land"| "lor" | "lxor" | ([/ % *] OperatorChar* !>> [! $ % & * + \- . / : \< = \> ? @ ^ | ~]) \ InfixSymbol2Exclude; 
lexical InfixSymbol3 = ([+ \-] OperatorChar* !>> [! $ % & * + \- . / : \< = \> ? @ ^ | ~]) \ InfixSymbol3Exclude;
lexical InfixSymbol4 = [@ ^] OperatorChar* !>> [! $ % & * + \- . / : \< = \> ? @ ^ | ~];
lexical InfixSymbol5 = ([= \< \> | & $] OperatorChar* !>> [! $ % & * + \- . / : \< = \> ? @ ^ | ~]) \ InfixSymbol5Exclude;
lexical InfixSymbol6 =  "&" | "&&";                      
lexical InfixSymbol7 =  "||" | "or";
lexical InfixSymbol8 =  ":=";

keyword InfixSymbol1Exclude = ")";
keyword InfixSymbol2Exclude = "**";
keyword InfixSymbol3Exclude = "-\>";
keyword InfixSymbol5Exclude = "|" | "||" | "&&" | "&" | "\<-";

keyword Keywords = "_" |
        "and"|       "as"|           "assert"|      "asr"|           "begin"|    
        "class"|     "constraint"|   "do"|          "done"|          "downto"|   
        "else"|      "end"|          "exception"|   "external"|      "false"|    
        "for"|       "fun"|          "function"|    "functor"|       "if"|       
        "in"|        "include"|      "inherit"|     "initializer"|   "land"|     
        "lazy"|      "let"|          "lor"|         "lsl"|           "lsr"|     
        "lxor"|      "match"|        "method"|      "mod"|           "module"|   
        "mutable"|   "new"|          "object"|      "of"|            "open"|     
        "or"|        "private"|      "rec"|         "sig"|           "struct"|   
        "then"|      "to"|           "true"|        "try"|           "type"|     
        "val"|       "virtual"|      "when"|        "while"|         "with";


lexical Comment = "(*" (![(*] | Comment | "*" !>> [)] | "(" !>> [*])* "*)";         
	
lexical Whitespace = [\ \t\n\r \u0009-\u000D];
	
layout Layout = (Comment | Whitespace)* !>> [\ \t\n\r \u0009-\u000D] !>> "(*";
