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
    =  { Id "."}+
    ;

syntax Ids               
    =  { Id ","}+
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
    =  FunctionArgTypes "=>" Type
    |  InfixType ExistentialClause?
    ;

syntax FunctionArgTypes  
    = InfixType
    | "(" { ParamType "," }* ] ")"
    ;

syntax ExistentialClause 
    =  "forSome" "{" { ExistentialDcl semi}+ "}"
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
    =  NL? "{" { RefineStat semi }+ "}"
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
    =  (Bindings | "implicit"? Id | "_") "=>" Expr
    |  Expr1
    ;

syntax Expr1             
    =  `if' `(' Expr `)' {nl} Expr [[semi] `else' Expr]
    |  `while' `(' Expr `)' {nl} Expr
    |  `try' (`{' Block `}' | Expr) [`catch' `{' CaseClauses `}'] [`finally' Expr]
    |  `do' Expr [semi] `while' `(' Expr ')'
    |  `for' (`(' Enumerators `)' | `{' Enumerators `}') {nl} [`yield'] Expr
    |  `throw' Expr
    |  `return' [Expr]
    |  [SimpleExpr `.'] Id `=' Expr
    |  SimpleExpr1 ArgumentExprs `=' Expr
    |  PostfixExpr
    |  PostfixExpr Ascription
    |  PostfixExpr `match' `{' CaseClauses `}'
    ;

syntax PostfixExpr       
    =  InfixExpr [Id [nl]]
    ;

syntax InfixExpr         
    =  PrefixExpr
    |  InfixExpr Id [nl] InfixExpr
    ;

syntax PrefixExpr        
    =  ["-" | "+" | "~" | "!"] SimpleExpr
    ;

syntax SimpleExpr        
    =  "new" (ClassTemplate | TemplateBody)
    |  BlockExpr
    |  SimpleExpr1 ["_"]
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
    =  Expr {"," Expr}
    ;

syntax ArgumentExprs     
    =  "(" [Exprs] ")"
    |  "(" [Exprs ","] PostfixExpr ":" "_" "*" ")"
    |  [nl] BlockExpr
    ;

syntax BlockExpr         
    =  "{" CaseClauses "}"
    |  "{" Block "}"
    ;

syntax Block             
    =  BlockStat {semi BlockStat} [ResultExpr]
    ;

syntax BlockStat         
    =  Import
    |  {Annotation} ["implicit" | "lazy"] Def
    |  {Annotation} {LocalModifier} TmplDef
    |  Expr1
    |
    ;

syntax ResultExpr        
    =  Expr1
    |  (Bindings | (["implicit"] Id | "_") ":" CompoundType) "=>" Block
    ;

syntax Enumerators       
    =  Generator {semi Generator}
    ;

syntax Generator         
    =  Pattern1 "<-" Expr {[semi] Guard | semi Pattern1 "=" Expr}
    ;

syntax CaseClauses       
    =  CaseClause { CaseClause }
    ;

syntax CaseClause        
    =  "case" Pattern [Guard] "=>" Block
    ;

syntax Guard             
    =  "if" PostfixExpr
    ;

syntax Pattern           
    =  Pattern1 { "|" Pattern1 }
    ;

syntax Pattern1          
    =  varId ":" TypePat
    |  "_" ":" TypePat
    |  Pattern2
    ;

syntax Pattern2          
    =  varId ["@" Pattern3]
    |  Pattern3
    ;

syntax Pattern3          
    =  SimplePattern
    |  SimplePattern { Id [nl] SimplePattern }
    ;

syntax SimplePattern     
    =  "_"
    |  varId
    |  Literal
    |  StableId
    |  StableId "(" [Patterns ")"
    |  StableId "(" [Patterns ","] [varId "@"] "_" "*" ")"
    |  "(" [Patterns] ")"
    |  XmlPattern
    ;

syntax Patterns          
    =  Pattern ["," Patterns]
    |  "_" *
    ;

syntax TypeParamClause   
    =  "[" VariantTypeParam {"," VariantTypeParam} "]"
    ;

syntax FunTypeParamClause
    =  "[" TypeParam {"," TypeParam} "]"
    ;

syntax VariantTypeParam  
    =  {Annotation} ["+" | "-"] TypeParam
    ;

syntax TypeParam         
    =  (Id | "_") [TypeParamClause] [">:" Type] ["<:" Type] {"<%" Type} {":" Type}
    ;

syntax ParamClauses      
    =  {ParamClause} [[nl] "(" "implicit" Params ")"]
    ;

syntax ParamClause       
    =  [nl] "(" [Params] ")"
    ;

syntax Params            
    =  Param {"," Param}
    ;

syntax Param             
    =  {Annotation} Id [":" ParamType] ["=" Expr]
    ;

syntax ParamType         
    =  Type
    |  "=>" Type
    |  Type "*"
    ;

syntax ClassParamClauses 
    =  {ClassParamClause} [[nl] "(" "implicit" ClassParams ")"]
    ;

syntax ClassParamClause  
    =  [nl] "(" [ClassParams] ")"
    ;

syntax ClassParams       
    =  ClassParam {"," ClassParam}
    ;

syntax ClassParam        
    =  {Annotation} {Modifier} [(`val' | `var')] Id ":" ParamType ["=" Expr]
    ;

syntax Bindings          
    =  "(" Binding {"," Binding} ")"
    ;

syntax Binding           
    =  (Id | "_") [":" Type]
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
    =  ("private" | "protected") [AccessQualifier]
    ;

syntax AccessQualifier   
    =  "[" (Id | "this") "]"
    ;

syntax Annotation        
    =  "@" SimpleType {ArgumentExprs}
    ;

syntax ConstrAnnotation  
    =  "@" SimpleType ArgumentExprs
    ;

syntax TemplateBody      
    =  [nl] "{" [SelfType] TemplateStat {semi TemplateStat} "}"
    ;

syntax TemplateStat      
    =  Import
    |  {Annotation [nl]} {Modifier} Def
    |  {Annotation [nl]} {Modifier} Dcl
    |  Expr
    |
    ;

syntax SelfType          
    =  Id [":" Type] "=>"
    |  "this" ":" Type "=>"
    ;

syntax Import            
    =  "import" ImportExpr {"," ImportExpr}
    ;

syntax ImportExpr        
    =  StableId "." (Id | "_" | ImportSelectors)
    ;

syntax ImportSelectors   
    =  "{" {ImportSelector ","} (ImportSelector | "_") "}"
    ;

syntax ImportSelector    
    =  Id ["=>" Id | "=>" "_"]
    ;

syntax Dcl               
    =  "val" ValDcl
    |  "var" VarDcl
    |  "def" FunDcl
    |  "type" {nl} TypeDcl
    ;

syntax ValDcl            
    =  Ids ":" Type
    ;

syntax VarDcl            
    =  Ids ":" Type
    ;

syntax FunDcl            
    =  FunSig [":" Type]
    ;

syntax FunSig            
    =  Id [FunTypeParamClause] ParamClauses
    ;

syntax TypeDcl           
    =  Id [TypeParamClause] [">:" Type] ["<:" Type]
    ;

syntax PatVarDef         
    =  "val" PatDef
    |  "var" VarDef
    ;

syntax Def               
    =  PatVarDef
    |  "def" FunDef
    |  "type" {nl} TypeDef
    |  TmplDef
    ;

syntax PatDef            
    =  Pattern2 {"," Pattern2} [":" Type] "=" Expr
    ;

syntax VarDef            
    =  PatDef
    |  Ids ":" Type "=" "_"
    ;

syntax FunDef            
    =  FunSig [":" Type] "=" Expr
    |  FunSig [nl] "{" Block "}"
    |  "this" ParamClause ParamClauses ("=" ConstrExpr | [nl] ConstrBlock)
    ;

syntax TypeDef           
    =  Id [TypeParamClause] "=" Type
    ;

syntax TmplDef           
    =  ["case"] "class" ClassDef
    |  ["case"] "object" ObjectDef
    |  "trait" TraitDef
    ;

syntax ClassDef          
    =  Id [TypeParamClause] {ConstrAnnotation} [AccessModifier] ClassParamClauses ClassTemplateOpt
    ;

syntax TraitDef          
    =  Id [TypeParamClause] TraitTemplateOpt
    ;

syntax ObjectDef         
    =  Id ClassTemplateOpt
    ;

syntax ClassTemplateOpt  
    =  "extends" ClassTemplate | [["extends"] TemplateBody]
    ;

syntax TraitTemplateOpt  
    =  "extends" TraitTemplate | [["extends"] TemplateBody]
    ;

syntax ClassTemplate     
    =  [EarlyDefs] ClassParents [TemplateBody]
    ;

syntax TraitTemplate     
    =  [EarlyDefs] TraitParents [TemplateBody]
    ;

syntax ClassParents      
    =  Constr {"with" AnnotType}
    ;

syntax TraitParents      
    =  AnnotType {"with" AnnotType}
    ;

syntax Constr            
    =  AnnotType {ArgumentExprs}
    ;

syntax EarlyDefs         
    = "{" [EarlyDef {semi EarlyDef}] "}" "with"
    ;

syntax EarlyDef          
    =  {Annotation [nl]} {Modifier} PatVarDef
    ;

syntax ConstrExpr        
    =  SelfInvocation
    |  ConstrBlock
    ;

syntax ConstrBlock       
    =  "{" SelfInvocation {semi BlockStat} "}"
    ;

syntax SelfInvocation    
    =  "this" ArgumentExprs {ArgumentExprs}
    ;

syntax TopStatSeq        
    =  TopStat {semi TopStat}
    ;

syntax TopStat           
    =  {Annotation [nl]} {Modifier} TmplDef
    |  Import
    |  Packaging
    |  PackageObject
    |
    ;

syntax Packaging         
    =  "package" QualId [nl] "{" TopStatSeq "}"
    ;

syntax PackageObject     
    =  "package" "object" ObjectDef
    ;

syntax CompilationUnit   
    =  {"package" QualId semi} TopStatSeq 
    ;
