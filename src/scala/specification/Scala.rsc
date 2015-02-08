/**
 *  Derived from the Scala Language Specification 2.11
 *
 *  http://www.scala-lang.org/files/archive/spec/2.11/13-syntax-summary.html
 *
 *  author: Ali Afroozeh
 */

module scala::specification::Scala

extend scala::specification::Lexical;
extend scala::specification::XML;

syntax Literal           
    =  "-"? IntegerLiteral
    |  "-"? FloatingPointLiteral
    |  BooleanLiteral
    |  CharacterLiteral
    |  StringLiteral
    |  SymbolLiteral
    |  "null"
    ;

syntax QualId            
    =  { Id "." }+
    ;

syntax Ids               
    =  { Id "," }+
    ;

syntax Path              
    =  StableId
    |  (Id ".")? "this"
    ;

syntax StableId          
    =  Id
    |  Path "." Id
    |  (Id ".")? "super" ClassQualifier? "." Id
    ;

syntax ClassQualifier    
    =  "[" Id "]"
    ;

syntax Type              
    =  FunctionArgTypes "=\>" Type
    |  InfixType ExistentialClause?
    ;

syntax FunctionArgTypes  
    = InfixType
    | "(" { ParamType "," }* ")"
    ;

syntax ExistentialClause 
    =  "forSome" "{" { ExistentialDcl Semi}+ "}"
    ;

syntax ExistentialDcl    
    =  "type" TypeDcl
    |  "val" ValDcl
    ;

syntax InfixType         
    =  CompoundType (Id NL? CompoundType)*
    ;

syntax CompoundType      
    =  { AnnotType "with" }+ Refinement?
    |  Refinement
    ;

syntax AnnotType         
    =  SimpleType Annotation*
    ;

syntax SimpleType        
    =  SimpleType TypeArgs
    |  SimpleType "#" Id
    |  StableId
    |  Path "." "type"
    |  "(" Types ")"
    ;

syntax TypeArgs          
    =  "[" Types "]"
    ;

syntax Types             
    =  { Type ","}+
    ;

syntax Refinement        
    =  NL? "{" { RefineStat Semi }+ "}"
    ;

syntax RefineStat        
    =  Dcl
    |  "type" TypeDef
    |
    ;

syntax TypePat           
    =  Type
    ;

syntax Ascription        
    =  ":" InfixType
    |  ":" Annotation+
    |  ":" "_" "*"
    ;

syntax Expr              
    =  (Bindings | ("implicit"? Id) | "_") "=\>" Expr
    |  Expr1
    ;

syntax Expr1             
    =  "if" "(" Expr ")" NL* Expr (Semi? "else" Expr)?
    |  "while" "(" Expr ")" NL* Expr
    |  "try" (("{" Block "}") | Expr) ("catch" "{" CaseClauses "}")? ("finally" Expr)?
    |  "do" Expr Semi? "while" "(" Expr ")"
    |  "for" ( ("(" Enumerators ")") | ("{" Enumerators "}")) NL* "yield"? Expr
    |  "throw" Expr
    |  "return" Expr?
    |  (SimpleExpr ".")? Id "=" Expr
    |  SimpleExpr1 ArgumentExprs "=" Expr
    |  PostfixExpr
    |  PostfixExpr Ascription
    |  PostfixExpr "match" "{" CaseClauses "}"
    ;

syntax PostfixExpr       
    =  InfixExpr (Id NL?)?
    ;

syntax InfixExpr         
    =  PrefixExpr
    |  InfixExpr Id NL? InfixExpr
    ;

syntax PrefixExpr        
    =  ("-" | "+" | "~" | "!")? SimpleExpr
    ;

syntax SimpleExpr        
    =  "new" (ClassTemplate | TemplateBody)
    |  BlockExpr
    |  SimpleExpr1 "_"?
    ;

syntax SimpleExpr1       
    =  Literal
    |  Path
    |  "_"
    |  "(" [Exprs] ")"
    |  SimpleExpr "." Id
    |  SimpleExpr TypeArgs
    |  SimpleExpr1 ArgumentExprs
    |  XmlExpr
    ;

syntax Exprs             
    =  { Expr ","}+
    ;

syntax ArgumentExprs     
    =  "(" Exprs? ")"
    |  "(" (Exprs ",")? PostfixExpr ":" "_" "*" ")"
    |  NL? BlockExpr
    ;

syntax BlockExpr         
    =  "{" CaseClauses "}"
    |  "{" Block "}"
    ;

syntax Block             
    =  { BlockStat Semi }+ ResultExpr?
    ;

syntax BlockStat         
    =  Import
    |  Annotation* ("implicit" | "lazy")? Def
    |  Annotation* LocalModifier* TmplDef
    |  Expr1
    |
    ;

syntax ResultExpr        
    =  Expr1
    |  (Bindings | ((("implicit"? Id) | "_") ":" CompoundType)) "=\>" Block
    ;

syntax Enumerators       
    =  {Generator Semi}+
    ;

syntax Generator         
    =  Pattern1 "\<-" Expr ((Semi? Guard) | (Semi Pattern1 "=" Expr))*
    ;

syntax CaseClauses       
    =  CaseClause+
    ;

syntax CaseClause        
    =  "case" Pattern Guard? "=\>" Block
    ;

syntax Guard             
    =  "if" PostfixExpr
    ;

syntax Pattern           
    =  { Pattern1 "|" }+
    ;

syntax Pattern1          
    =  VarId ":" TypePat
    |  "_" ":" TypePat
    |  Pattern2
    ;

syntax Pattern2          
    =  VarId ("@" Pattern3)?
    |  Pattern3
    ;

syntax Pattern3          
    =  SimplePattern
    |  SimplePattern ( Id NL? SimplePattern )*
    ;

syntax SimplePattern     
    =  "_"
    |  VarId
    |  Literal
    |  StableId
    |  StableId "(" Patterns? ")"
    |  StableId "(" (Patterns ",")? (VarId "@")? "_" "*" ")"
    |  "(" Patterns? ")"
    |  XmlPattern
    ;

syntax Patterns          
    =  Pattern ("," Patterns)?
    |  "_" *
    ;

syntax TypeParamClause   
    =  "[" { VariantTypeParam "," }+ "]"
    ;

syntax FunTypeParamClause
    =  "[" { TypeParam "," }+ "]"
    ;

syntax VariantTypeParam  
    =  Annotation* ("+" | "-")? TypeParam
    ;

syntax TypeParam         
    =  (Id | "_") TypeParamClause? ("\>:" Type)? ("\<:" Type)? ("\<%" Type)* (":" Type)*
    ;

syntax ParamClauses      
    =  ParamClause* (NL? "(" "implicit" Params ")")?
    ;

syntax ParamClause       
    =  NL? "(" Params? ")"
    ;

syntax Params            
    =  { Param "," }+
    ;

syntax Param             
    =  Annotation* Id (":" ParamType)? ("=" Expr)?
    ;

syntax ParamType         
    =  Type
    |  "=\>" Type
    |  Type "*"
    ;

syntax ClassParamClauses 
    =  ClassParamClause* (NL? "(" "implicit" ClassParams ")")?
    ;

syntax ClassParamClause  
    =  NL? "(" ClassParams? ")"
    ;

syntax ClassParams       
    =  {ClassParam ","}+
    ;

syntax ClassParam        
    =  Annotation* Modifier* ("val" | "var")? Id ":" ParamType ("=" Expr)?
    ;

syntax Bindings          
    =  "(" { Binding "," }+ ")"
    ;

syntax Binding           
    =  (Id | "_") (":" Type)?
    ;

syntax Modifier          
    =  LocalModifier
    |  AccessModifier
    |  "overrIde"
    ;

syntax LocalModifier     
    =  "abstract"
    |  "final"
    |  "sealed"
    |  "implicit"
    |  "lazy"
    ;

syntax AccessModifier    
    =  ("private" | "protected") AccessQualifier?
    ;

syntax AccessQualifier   
    =  "[" (Id | "this") "]"
    ;

syntax Annotation        
    =  "@" SimpleType ArgumentExprs*
    ;

syntax ConstrAnnotation  
    =  "@" SimpleType ArgumentExprs
    ;

syntax TemplateBody      
    =  NL? "{" SelfType? { TemplateStat Semi}+ "}"
    ;

syntax TemplateStat      
    =  Import
    |  (Annotation NL?)* Modifier* Def
    |  (Annotation NL?)* Modifier* Dcl
    |  Expr
    |
    ;

syntax SelfType          
    =  Id (":" Type)? "=\>"
    |  "this" ":" Type "=\>"
    ;

syntax Import            
    =  "import" { ImportExpr ","}+
    ;

syntax ImportExpr        
    =  StableId "." (Id | "_" | ImportSelectors)
    ;

syntax ImportSelectors   
    =  "{" (ImportSelector ",")* (ImportSelector | "_") "}"
    ;

syntax ImportSelector    
    =  Id (("=\>" Id) | ("=\>" "_"))?
    ;

syntax Dcl               
    =  "val" ValDcl
    |  "var" VarDcl
    |  "def" FunDcl
    |  "type" NL* TypeDcl
    ;

syntax ValDcl            
    =  Ids ":" Type
    ;

syntax VarDcl            
    =  Ids ":" Type
    ;

syntax FunDcl            
    =  FunSig (":" Type)?
    ;

syntax FunSig            
    =  Id FunTypeParamClause? ParamClauses
    ;

syntax TypeDcl           
    =  Id TypeParamClause? ("\>:" Type)? ("\<:" Type)?
    ;

syntax PatVarDef         
    =  "val" PatDef
    |  "var" VarDef
    ;

syntax Def               
    =  PatVarDef
    |  "def" FunDef
    |  "type" NL* TypeDef
    |  TmplDef
    ;

syntax PatDef            
    =  { Pattern2 "," }+ (":" Type)? "=" Expr
    ;

syntax VarDef            
    =  PatDef
    |  Ids ":" Type "=" "_"
    ;

syntax FunDef            
    =  FunSig (":" Type)? "=" Expr
    |  FunSig NL? "{" Block "}"
    |  "this" ParamClause ParamClauses (("=" ConstrExpr) | (NL? ConstrBlock))
    ;

syntax TypeDef           
    =  Id TypeParamClause? "=" Type
    ;

syntax TmplDef           
    =  "case"? "class" ClassDef
    |  "case"? "object" ObjectDef
    |  "trait" TraitDef
    ;

syntax ClassDef          
    =  Id TypeParamClause? ConstrAnnotation* AccessModifier? ClassParamClauses ClassTemplateOpt
    ;

syntax TraitDef          
    =  Id TypeParamClause? TraitTemplateOpt
    ;

syntax ObjectDef         
    =  Id ClassTemplateOpt
    ;

syntax ClassTemplateOpt  
    =  "extends" ClassTemplate | ("extends"? TemplateBody)?
    ;

syntax TraitTemplateOpt  
    =  "extends" TraitTemplate | ("extends"? TemplateBody)?
    ;

syntax ClassTemplate     
    =  EarlyDefs? ClassParents TemplateBody?
    ;

syntax TraitTemplate     
    =  EarlyDefs? TraitParents TemplateBody?
    ;

syntax ClassParents      
    =  Constr ("with" AnnotType)*
    ;

syntax TraitParents      
    =  AnnotType ("with" AnnotType)*
    ;

syntax Constr            
    =  AnnotType ArgumentExprs*
    ;

syntax EarlyDefs         
    = "{" { EarlyDef Semi }* "}" "with"
    ;

syntax EarlyDef          
    =  (Annotation NL?)* Modifier* PatVarDef
    ;

syntax ConstrExpr        
    =  SelfInvocation
    |  ConstrBlock
    ;

syntax ConstrBlock       
    =  "{" SelfInvocation (Semi BlockStat)* "}"
    ;

syntax SelfInvocation    
    =  "this" ArgumentExprs+
    ;

syntax TopStatSeq        
    =  { TopStat Semi }+
    ;

syntax TopStat           
    =  (Annotation NL?)? Modifier* TmplDef
    |  Import
    |  Packaging
    |  PackageObject
    |
    ;

syntax Packaging         
    =  "package" QualId NL? "{" TopStatSeq "}"
    ;

syntax PackageObject     
    =  "package" "object" ObjectDef
    ;

syntax CompilationUnit   
    =  ("package" QualId Semi)* TopStatSeq 
    ;
