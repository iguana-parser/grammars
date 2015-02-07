syntax Literal           
    =  ‘-’? integerLiteral
    |  ‘-’? floatingPointLiteral
    |  booleanLiteral
    |  characterLiteral
    |  stringLiteral
    |  symbolLiteral
    |  ‘null’
    ;

syntax QualId            
    =  id {‘.’ id}
    ;

syntax ids               
    =  id {‘,’ id}
    ;

syntax Path              
    =  StableId
    |  [id ‘.’] ‘this’
    ;

syntax StableId          
    =  id
    |  Path ‘.’ id
    |  [id ‘.’] ‘super’ [ClassQualifier] ‘.’ id
    ;

syntax ClassQualifier    
    =  ‘[’ id ‘]’
    ;

syntax Type              
    =  FunctionArgTypes ‘=>’ Type
    |  InfixType [ExistentialClause]
    ;

syntax FunctionArgTypes  
    = InfixType
    | ‘(’ [ ParamType {‘,’ ParamType } ] ‘)’
    ;

syntax ExistentialClause 
    =  ‘forSome’ ‘{’ ExistentialDcl {semi ExistentialDcl} ‘}’
    ;

syntax ExistentialDcl    
    =  ‘type’ TypeDcl
    |  ‘val’ ValDcl
    ;

syntax InfixType         
    =  CompoundType {id [nl] CompoundType}
    ;

syntax CompoundType      
    =  AnnotType {‘with’ AnnotType} [Refinement]
    |  Refinement
    ;

syntax AnnotType         
    =  SimpleType {Annotation}
    ;

syntax SimpleType        
    =  SimpleType TypeArgs
    |  SimpleType ‘#’ id
    |  StableId
    |  Path ‘.’ ‘type’
    |  ‘(’ Types ‘)’
    ;

syntax TypeArgs          
    =  ‘[’ Types ‘]’
    ;

syntax Types             
    =  Type {‘,’ Type}
    ;

syntax Refinement        
    =  [nl] ‘{’ RefineStat {semi RefineStat} ‘}’
    ;

syntax RefineStat        
    =  Dcl
    |  ‘type’ TypeDef
    |
    ;

syntax TypePat           
    =  Type
    ;

syntax Ascription        
    =  ‘:’ InfixType
    |  ‘:’ Annotation {Annotation}
    |  ‘:’ ‘_’ ‘*’
    ;

syntax Expr              
    =  (Bindings | [‘implicit’] id | ‘_’) ‘=>’ Expr
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
    |  [SimpleExpr `.'] id `=' Expr
    |  SimpleExpr1 ArgumentExprs `=' Expr
    |  PostfixExpr
    |  PostfixExpr Ascription
    |  PostfixExpr `match' `{' CaseClauses `}'
    ;

syntax PostfixExpr       
    =  InfixExpr [id [nl]]
    ;

syntax InfixExpr         
    =  PrefixExpr
    |  InfixExpr id [nl] InfixExpr
    ;

syntax PrefixExpr        
    =  [‘-’ | ‘+’ | ‘~’ | ‘!’] SimpleExpr
    ;

syntax SimpleExpr        
    =  ‘new’ (ClassTemplate | TemplateBody)
    |  BlockExpr
    |  SimpleExpr1 [‘_’]
    ;

syntax SimpleExpr1       
    =  Literal
    |  Path
    |  ‘_’
    |  ‘(’ [Exprs] ‘)’
    |  SimpleExpr ‘.’ id
    |  SimpleExpr TypeArgs
    |  SimpleExpr1 ArgumentExprs
    |  XmlExpr
    ;

syntax Exprs             
    =  Expr {‘,’ Expr}
    ;

syntax ArgumentExprs     
    =  ‘(’ [Exprs] ‘)’
    |  ‘(’ [Exprs ‘,’] PostfixExpr ‘:’ ‘_’ ‘*’ ‘)’
    |  [nl] BlockExpr
    ;

syntax BlockExpr         
    =  ‘{’ CaseClauses ‘}’
    |  ‘{’ Block ‘}’
    ;

syntax Block             
    =  BlockStat {semi BlockStat} [ResultExpr]
    ;

syntax BlockStat         
    =  Import
    |  {Annotation} [‘implicit’ | ‘lazy’] Def
    |  {Annotation} {LocalModifier} TmplDef
    |  Expr1
    |
    ;

syntax ResultExpr        
    =  Expr1
    |  (Bindings | ([‘implicit’] id | ‘_’) ‘:’ CompoundType) ‘=>’ Block
    ;

syntax Enumerators       
    =  Generator {semi Generator}
    ;

syntax Generator         
    =  Pattern1 ‘<-’ Expr {[semi] Guard | semi Pattern1 ‘=’ Expr}
    ;

syntax CaseClauses       
    =  CaseClause { CaseClause }
    ;

syntax CaseClause        
    =  ‘case’ Pattern [Guard] ‘=>’ Block
    ;

syntax Guard             
    =  ‘if’ PostfixExpr
    ;

syntax Pattern           
    =  Pattern1 { ‘|’ Pattern1 }
    ;

syntax Pattern1          
    =  varid ‘:’ TypePat
    |  ‘_’ ‘:’ TypePat
    |  Pattern2
    ;

syntax Pattern2          
    =  varid [‘@’ Pattern3]
    |  Pattern3
    ;

syntax Pattern3          
    =  SimplePattern
    |  SimplePattern { id [nl] SimplePattern }
    ;

syntax SimplePattern     
    =  ‘_’
    |  varid
    |  Literal
    |  StableId
    |  StableId ‘(’ [Patterns ‘)’
    |  StableId ‘(’ [Patterns ‘,’] [varid ‘@’] ‘_’ ‘*’ ‘)’
    |  ‘(’ [Patterns] ‘)’
    |  XmlPattern
    ;

syntax Patterns          
    =  Pattern [‘,’ Patterns]
    |  ‘_’ *
    ;

syntax TypeParamClause   
    =  ‘[’ VariantTypeParam {‘,’ VariantTypeParam} ‘]’
    ;

syntax FunTypeParamClause
    =  ‘[’ TypeParam {‘,’ TypeParam} ‘]’
    ;

syntax VariantTypeParam  
    =  {Annotation} [‘+’ | ‘-’] TypeParam
    ;

syntax TypeParam         
    =  (id | ‘_’) [TypeParamClause] [‘>:’ Type] [‘<:’ Type] {‘<%’ Type} {‘:’ Type}
    ;

syntax ParamClauses      
    =  {ParamClause} [[nl] ‘(’ ‘implicit’ Params ‘)’]
    ;

syntax ParamClause       
    =  [nl] ‘(’ [Params] ‘)’
    ;

syntax Params            
    =  Param {‘,’ Param}
    ;

syntax Param             
    =  {Annotation} id [‘:’ ParamType] [‘=’ Expr]
    ;

syntax ParamType         
    =  Type
    |  ‘=>’ Type
    |  Type ‘*’
    ;

syntax ClassParamClauses 
    =  {ClassParamClause} [[nl] ‘(’ ‘implicit’ ClassParams ‘)’]
    ;

syntax ClassParamClause  
    =  [nl] ‘(’ [ClassParams] ‘)’
    ;

syntax ClassParams       
    =  ClassParam {‘,’ ClassParam}
    ;

syntax ClassParam        
    =  {Annotation} {Modifier} [(`val' | `var')] id ‘:’ ParamType [‘=’ Expr]
    ;

syntax Bindings          
    =  ‘(’ Binding {‘,’ Binding} ‘)’
    ;

syntax Binding           
    =  (id | ‘_’) [‘:’ Type]
    ;

syntax Modifier          
    =  LocalModifier
    |  AccessModifier
    |  ‘override’
    ;

syntax LocalModifier     
    =  ‘abstract’
    |  ‘final’
    |  ‘sealed’
    |  ‘implicit’
    |  ‘lazy’
    ;

syntax AccessModifier    
    =  (‘private’ | ‘protected’) [AccessQualifier]
    ;

syntax AccessQualifier   
    =  ‘[’ (id | ‘this’) ‘]’
    ;

syntax Annotation        
    =  ‘@’ SimpleType {ArgumentExprs}
    ;

syntax ConstrAnnotation  
    =  ‘@’ SimpleType ArgumentExprs
    ;

syntax TemplateBody      
    =  [nl] ‘{’ [SelfType] TemplateStat {semi TemplateStat} ‘}’
    ;

syntax TemplateStat      
    =  Import
    |  {Annotation [nl]} {Modifier} Def
    |  {Annotation [nl]} {Modifier} Dcl
    |  Expr
    |
    ;

syntax SelfType          
    =  id [‘:’ Type] ‘=>’
    |  ‘this’ ‘:’ Type ‘=>’
    ;

syntax Import            
    =  ‘import’ ImportExpr {‘,’ ImportExpr}
    ;

syntax ImportExpr        
    =  StableId ‘.’ (id | ‘_’ | ImportSelectors)
    ;

syntax ImportSelectors   
    =  ‘{’ {ImportSelector ‘,’} (ImportSelector | ‘_’) ‘}’
    ;

syntax ImportSelector    
    =  id [‘=>’ id | ‘=>’ ‘_’]
    ;

syntax Dcl               
    =  ‘val’ ValDcl
    |  ‘var’ VarDcl
    |  ‘def’ FunDcl
    |  ‘type’ {nl} TypeDcl
    ;

syntax ValDcl            
    =  ids ‘:’ Type
    ;

syntax VarDcl            
    =  ids ‘:’ Type
    ;

syntax FunDcl            
    =  FunSig [‘:’ Type]
    ;

syntax FunSig            
    =  id [FunTypeParamClause] ParamClauses
    ;

syntax TypeDcl           
    =  id [TypeParamClause] [‘>:’ Type] [‘<:’ Type]
    ;

syntax PatVarDef         
    =  ‘val’ PatDef
    |  ‘var’ VarDef
    ;

syntax Def               
    =  PatVarDef
    |  ‘def’ FunDef
    |  ‘type’ {nl} TypeDef
    |  TmplDef
    ;

syntax PatDef            
    =  Pattern2 {‘,’ Pattern2} [‘:’ Type] ‘=’ Expr
    ;

syntax VarDef            
    =  PatDef
    |  ids ‘:’ Type ‘=’ ‘_’
    ;

syntax FunDef            
    =  FunSig [‘:’ Type] ‘=’ Expr
    |  FunSig [nl] ‘{’ Block ‘}’
    |  ‘this’ ParamClause ParamClauses (‘=’ ConstrExpr | [nl] ConstrBlock)
    ;

syntax TypeDef           
    =  id [TypeParamClause] ‘=’ Type
    ;

syntax TmplDef           
    =  [‘case’] ‘class’ ClassDef
    |  [‘case’] ‘object’ ObjectDef
    |  ‘trait’ TraitDef
    ;

syntax ClassDef          
    =  id [TypeParamClause] {ConstrAnnotation} [AccessModifier] ClassParamClauses ClassTemplateOpt
    ;

syntax TraitDef          
    =  id [TypeParamClause] TraitTemplateOpt
    ;

syntax ObjectDef         
    =  id ClassTemplateOpt
    ;

syntax ClassTemplateOpt  
    =  ‘extends’ ClassTemplate | [[‘extends’] TemplateBody]
    ;

syntax TraitTemplateOpt  
    =  ‘extends’ TraitTemplate | [[‘extends’] TemplateBody]
    ;

syntax ClassTemplate     
    =  [EarlyDefs] ClassParents [TemplateBody]
    ;

syntax TraitTemplate     
    =  [EarlyDefs] TraitParents [TemplateBody]
    ;

syntax ClassParents      
    =  Constr {‘with’ AnnotType}
    ;

syntax TraitParents      
    =  AnnotType {‘with’ AnnotType}
    ;

syntax Constr            
    =  AnnotType {ArgumentExprs}
    ;

syntax EarlyDefs         
    = ‘{’ [EarlyDef {semi EarlyDef}] ‘}’ ‘with’
    ;

syntax EarlyDef          
    =  {Annotation [nl]} {Modifier} PatVarDef
    ;

syntax ConstrExpr        
    =  SelfInvocation
    |  ConstrBlock
    ;

syntax ConstrBlock       
    =  ‘{’ SelfInvocation {semi BlockStat} ‘}’
    ;

syntax SelfInvocation    
    =  ‘this’ ArgumentExprs {ArgumentExprs}
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
    =  ‘package’ QualId [nl] ‘{’ TopStatSeq ‘}’
    ;

syntax PackageObject     
    =  ‘package’ ‘object’ ObjectDef
    ;

syntax CompilationUnit   
    =  {‘package’ QualId semi} TopStatSeq 
    ;
