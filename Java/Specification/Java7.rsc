/**
 *  Derived from http://docs.oracle.com/javase/specs/jls/se7/html/jls-18.html
 *
 *
 *
 *  author: Ali Afroozeh
 */
module Java7


syntax Identifier =
   [$ A-Z _ a-z] !<< ID \ IDKeywords !>> [$ 0-9 A-Z _ a-z] 
  ;

syntax QualifiedIdentifier =
  		     {Identifier "."}+ 
  	     ;

//----------------------------------------------------------------------------------------------------------------

start syntax CompilationUnit =
   PackageDeclaration? ImportDeclaration* TypeDeclaration*
  ;
  
  
syntax PackageDeclaration =
   Annotations* "package"  QualifiedIdentifier ";" 
  ;  
  
syntax ImportDeclaration 
  = "import"  "static"?  {Identifier "."}+ ("." "*")? ";" 
  ;
  
syntax TypeDeclaration =
	  ClassOrInterfaceDeclaration ";" 
  ;  
  
syntax ClassOrInterfaceDeclaration = 
	   Modifier* (InterfaceDeclaration | ClassDeclaration); 
  
syntax ClassDeclaration =
  ClassDecHead ClassBody
  | EnumDeclaration 
  ;
  
syntax InterfaceDeclaration = 
         NormalInterfaceDeclaration
       | AnnotationTypeDeclaration;
  
  
syntax NormalClassDeclaration =
     "class" Identifier TypeParameters? ("extends" Type)? ("implements" TypeList) ClassBody;
                                
syntax EnumDeclaration =
    "enum" Identifier ("implements" TypeList)? EnumBody;
                                
syntax NormalInterfaceDeclaration = 
    "interface" Identifier TypeParameters? ("extends" TypeList)? InterfaceBody;

syntax AnnotationTypeDeclaration =
    "@" "interface" Identifier AnnotationTypeBody;
  
//----------------------------------------------------------------------------------------------------------------  
  
syntax Type =
       BasicType ("[" "]")* 
  	     | ReferenceType ("[" "]")*
  	     ;

syntax BasicType =
		    "byte"
    | "short"
    | "char"
    | "int"
    | "long"
    | "float"
    | "double"
    | "boolean"
    ;
  
syntax ReferenceType =
    Identifier TypeArguments? ( "." Identifier TypeArguments? )*;
  
syntax TypeArguments =
   "\<" {TypeArgument ","}+ "\>" 
  	  ;
  	  
syntax TypeArgument =  
       ReferenceType
       | "?" (("extends" | "super") ReferenceType)?
       ;
  
//----------------------------------------------------------------------------------------------------------------    
  
syntax NonWildcardTypeArguments =
    "\<" TypeList "\>"
    ;

syntax TypeList =  
    ReferenceType ("," ReferenceType)*
    ;


syntax TypeArgumentsOrDiamond =
    "\<" "\>" 
    | TypeArguments
    ;

syntax NonWildcardTypeArgumentsOrDiamond =
    "\<" "\>" 
    | NonWildcardTypeArguments
    ;

syntax TypeParameters =
    "\<" {TypeParameter ","}+ "\>"
    ;

syntax TypeParameter =
    Identifier ("extends" Bound)?
    ;

syntax Bound =  
    {ReferenceType "&"}+
    ;  
    
//----------------------------------------------------------------------------------------------------------------    

syntax Modifier =
  "final"
  | "strictfp" 
  | "private" 
  | "synchronized" 
  | "volatile" 
  | "protected" 
  | "transient" 
  | "abstract" 
  | "native" 
  | "static" 
  | "public" 
  ;

  
syntax Annotation =
   "@" QualifiedIdentifier  ( "(" AnnotationElement? ")" )?
  ;

syntax AnnotationElement =
       ElementValuePairs
       | ElementValue
       ;

syntax ElementValuePairs =
    {ElementValuePair ","}+
    ;

syntax ElementValuePair =
    Identifier "=" ElementValue;


syntax ElementValue =
        Annotation
        | Expression1
        | ElementValueArrayInitializer
        ;

syntax   ElementValueArrayInitializer =
        "{" ElementValues? ","? "}"
        ;

syntax 	ElementValues =
       {ElementValue ","}+
       ;

//----------------------------------------------------------------------------------------------------------------

syntax ClassBody = 
    "{" ClassBodyDeclaration* "}"
   	 ;

syntax ClassBodyDeclaration = 
    ";" 
    | Modifier* MemberDecl
    | "static"? Block
    ;

syntax MemberDecl = 
    MethodOrFieldDecl
    | "void" Identifier VoidMethodDeclaratorRest
    | Identifier ConstructorDeclaratorRest
    | GenericMethodOrConstructorDecl
    | ClassDeclaration
    | InterfaceDeclaration
    ;
    

syntax MethodOrFieldDecl =
    Type Identifier MethodOrFieldRest
    ;

syntax MethodOrFieldRest =  
    FieldDeclaratorsRest ";"
    | MethodDeclaratorRest
    ;

syntax FieldDeclaratorsRest =  
    VariableDeclaratorRest ("," VariableDeclarator)*
    ;

syntax MethodDeclaratorRest =
    FormalParameters ("[""]")* ("throws" QualifiedIdentifierList)? (Block | ";")
    ;

syntax VoidMethodDeclaratorRest =
    FormalParameters ("throws" QualifiedIdentifierList)? (Block | ";")
    ;

syntax ConstructorDeclaratorRest =
    FormalParameters ("throws" QualifiedIdentifierList)? Block
    ;

syntax GenericMethodOrConstructorDecl = 
    TypeParameters GenericMethodOrConstructorRest;

syntax GenericMethodOrConstructorRest =
    (Type | "void") Identifier MethodDeclaratorRest
    Identifier ConstructorDeclaratorRest
    ;
    
//----------------------------------------------------------------------------------------------------------------

syntax InterfaceBody =  
    "{" InterfaceBodyDeclaration* "}"
    ;

syntax InterfaceBodyDeclaration =
    ";" 
    | Modifier* InterfaceMemberDecl
    ;

syntax InterfaceMemberDecl =
    InterfaceMethodOrFieldDecl
    | "void" Identifier VoidInterfaceMethodDeclaratorRest
    | InterfaceGenericMethodDecl
    | ClassDeclaration
    | InterfaceDeclaration
    ;

syntax InterfaceMethodOrFieldDecl =
    Type Identifier InterfaceMethodOrFieldRest
    ;

syntax InterfaceMethodOrFieldRest =
    ConstantDeclaratorsRest ";"
    | InterfaceMethodDeclaratorRest
    ;

syntax ConstantDeclaratorsRest = 
    ConstantDeclaratorRest ("," ConstantDeclarator)*
    ;

syntax ConstantDeclaratorRest =  
    ("[""]")* "=" VariableInitializer
    ;

syntax ConstantDeclarator = 
    Identifier ConstantDeclaratorRest
    ;

syntax InterfaceMethodDeclaratorRest =
    FormalParameters ("[""]")* ("throws" QualifiedIdentifierList)? ";"
    ; 

syntax VoidInterfaceMethodDeclaratorRest = 
    FormalParameters ("throws" QualifiedIdentifierList)? ";"
    ;  

syntax InterfaceGenericMethodDecl = 
    TypeParameters (Type | "void") Identifier InterfaceMethodDeclaratorRest
    ;

//----------------------------------------------------------------------------------------------------------------

syntax FormalParameters = 
    "(" FormalParameterDecls? ")"
    ;

syntax FormalParameterDecls = 
    VariableModifier*  Type FormalParameterDeclsRest
    ;

syntax VariableModifier = 
    "final"
    | Annotation
    ;

syntax FormalParameterDeclsRest =  
    VariableDeclaratorId ("," FormalParameterDecls)?
    | "..." VariableDeclaratorId
    ;

syntax VariableDeclaratorId = 
    Identifier ("[""]")*
    ;

syntax VariableDeclarators =
    {VariableDeclarator ","}+
    ;

syntax VariableDeclarator =
    Identifier VariableDeclaratorRest
    ;

syntax VariableDeclaratorRest = 
    ("[" "]")* ( "=" VariableInitializer)?
    ;

syntax VariableInitializer = 
    ArrayInitializer
    | Expression
    ;

syntax ArrayInitializer = 
    "{" ({VariableInitializer ","}+ ","? )? "}"
    ;

//----------------------------------------------------------------------------------------------------------------

syntax Block = 
    "{" BlockStatement* "}"
    ;

syntax BlockStatement = 
    LocalVariableDeclarationStatement
    | ClassOrInterfaceDeclaration
    | (Identifier ":")? Statement
    ;

syntax LocalVariableDeclarationStatement = 
    VariableModifier*  Type VariableDeclarators ";"
    ;

syntax Statement =
    Block
    | ";"
    | Identifier ":" Statement
    | StatementExpression ";"
    | "if" ParExpression Statement ("else" Statement)? 
    | "assert" Expression (":" Expression)? ";"
    | "switch" ParExpression "{" SwitchBlockStatementGroups "}" 
    | "while" ParExpression Statement
    | "do" Statement "while" ParExpression ";"
    | "for" "(" ForControl ")" Statement
    | "break" Identifier? ";"
    | "continue" Identifier? ";"
    | "return" [Expression] ";"
    | "throw" Expression ";"
    | "synchronized" ParExpression Block
    | "try" Block (CatcheClause+ | (CatchClause* Finally))
    | "try" ResourceSpecification Block CatcheClause* Finally?
    ;

syntax StatementExpression =  
    Expression
    ;
    
//----------------------------------------------------------------------------------------------------------------

syntax CatchClause =   
    "catch" "(" VariableModifier* CatchType Identifier ")" Block
    ;

syntax CatchType  = 
    {QualifiedIdentifier "|"}+
    ;

syntax Finally = 
    "finally" Block
    ;

syntax ResourceSpecification = 
    "(" Resources ";"? ")"
    ;

syntax Resources = 
    {Resource ";"}+
    ;

syntax Resource = 
    VariableModifier* ReferenceType VariableDeclaratorId "=" Expression
    ; 

//----------------------------------------------------------------------------------------------------------------

syntax SwitchBlockStatementGroups =  
    "{" SwitchBlockStatementGroup "}"
    ;

syntax SwitchBlockStatementGroup =  
    SwitchLabel+ BlockStatements
    ;

syntax SwitchLabel =  
    "case" Expression ":"
    | "case" EnumConstantName ":"
    | "default" ":"
    ;

syntax EnumConstantName = 
    Identifier
    ;

syntax ForControl = 
    ForVarControl
    | ForInit ";" Expression? ";" ForUpdate?
    ;

syntax ForVarControl = 
    VariableModifier* Type VariableDeclaratorId  ForVarControlRest
    ;

syntax ForVarControlRest = 
    ForVariableDeclaratorsRest ";" Expression? ";" ForUpdate?
    | ":" Expression
    ;

syntax ForVariableDeclaratorsRest =
    ("=" VariableInitializer)? ( "," VariableDeclarator)*
    ;

syntax ForInit = ;

syntax ForUpdate = 
    {StatementExpression ","}+
    ;    

//----------------------------------------------------------------------------------------------------------------
syntax Expression =  
    Expression1 (AssignmentOperator Expression1)?
    ;

syntax AssignmentOperator =  
    "=" 
    | "+="
    | "-=" 
    | "*="
    | "/="
    | "&="
    | "|="
    | "^="
    | "%="
    | "\<\<="
    | "\>\>="
    | "\>\>\>="
    ;

syntax Expression1 = 
    Expression2 Expression1Rest?
    ;

syntax Expression1Rest = 
    "?" Expression ":" Expression1
    ;

syntax Expression2 =
    Expression3 Expression2Rest?
    ;

syntax Expression2Rest =
    (InfixOp Expression3)*
    | "instanceof" Type
    ;

//----------------------------------------------------------------------------------------------------------------

syntax InfixOp = 
    "||" 
    | "&&"
    | "|"
    | "^"
    | "&"
    | "=="
    | "!="
    | "\<"
    | "\>"
    | "\<="
    | "\>="
    | "\<\<"
    | "\>\>"
    | "\>\>\>"
    | "+"
    | "-"
    | "*"
    | "/"
    | "%"
    ;

syntax Expression3 = 
    PrefixOp Expression3
    | "(" (Expression | Type) ")" Expression3
    | Primary Selector* PostfixOp*
    ;

syntax PrefixOp = 
    "++"
    | "--"
    | "!"
    | "~"
    | "+"
    | "-"
    ;

syntax PostfixOp =  
    "++"
    | "--"
    ;

//----------------------------------------------------------------------------------------------------------------

syntax Primary =  
    Literal
    | ParExpression
    | "this" Arguments?
    | "super" SuperSuffix
    | "new" Creator
    | NonWildcardTypeArguments (ExplicitGenericInvocationSuffix | ("this" Arguments))
    | Identifier ( "." Identifier) IdentifierSuffix?
    | BasicType ("[" "]")* "." "class"
    | "void" "." "class"
    ;


syntax Literal = 
    IntegerLiteral
    | FloatingPointLiteral
    | CharacterLiteral 	
    | StringLiteral 	
    | BooleanLiteral
    | NullLiteral
    ;

syntax ParExpression =  
    "(" Expression ")"
    ;

syntax Arguments =
    "(" {Expression ","}* ")"
    ;

syntax SuperSuffix =  
    Arguments 
    | "." Identifier Arguments?
    ;

syntax ExplicitGenericInvocationSuffix =  
    "super" SuperSuffix
    | Identifier Arguments
    ;
    
//----------------------------------------------------------------------------------------------------------------

syntax Creator =  
    NonWildcardTypeArguments CreatedName ClassCreatorRest
    CreatedName (ClassCreatorRest | ArrayCreatorRest)
    ;

syntax CreatedName =   
    Identifier TypeArgumentsOrDiamond? ("." Identifier TypeArgumentsOrDiamond?)*
    ;

syntax ClassCreatorRest = 
    Arguments ClassBody?
    ;

syntax ArrayCreatorRest =
		     "[" ( ("]" ("[""]")* ArrayInitializer)  |  (Expression "]" ("[" Expression "]")* ("[" "]")*) )
		    ;


syntax IdentifierSuffix = 
    "[" ((("[""]")* "." "class") | Expression) "]"
    | Arguments 
    | "." ("class" | ExplicitGenericInvocation | "this" | ("super" Arguments) |
                                ("new" NonWildcardTypeArguments? InnerCreator))
		      ;

syntax ExplicitGenericInvocation = 
    NonWildcardTypeArguments ExplicitGenericInvocationSuffix
    ;

syntax InnerCreator =  
    Identifier NonWildcardTypeArgumentsOrDiamond? ClassCreatorRest
    ;

syntax Selector = 
    "." Identifier Arguments?
    | "." ExplicitGenericInvocation
    | "." "this"
    | "." "super" SuperSuffix
    | "." "new" NonWildcardTypeArguments? InnerCreator
    | "[" Expression "]"
    ;

//----------------------------------------------------------------------------------------------------------------
syntax EnumBody = 
    "{" {EnumConstant ","}* ","? EnumBodyDeclarations? "}"
    ;

syntax EnumConstant =
    Annotations? Identifier Arguments? ClassBody?;

syntax EnumBodyDeclarations = 
    ";" ClassBodyDeclaration*
    ;

syntax AnnotationTypeBody = 
    "{" AnnotationTypeElementDeclaration+ "}"
    ;


syntax AnnotationTypeElementDeclaration = 
    Modifier* AnnotationTypeElementRest
    ;

syntax AnnotationTypeElementRest = 
    Type Identifier AnnotationMethodOrConstantRest ";"
    | ClassDeclaration
    | InterfaceDeclaration
    | EnumDeclaration  
    | AnnotationTypeDeclaration
    ;

syntax AnnotationMethodOrConstantRest = 
    AnnotationMethodRest
    | ConstantDeclaratorsRest
    ;  

syntax AnnotationMethodRest = 
    "(" ")" ("[""]")? ("default" ElementValue)?
    ;

//----------------------------------------------------------------------------------------------------------------


lexical SignedInteger =
  [+ \-]? [0-9]+ 
  ;

lexical LEX[StringLiteral] =
   "\"" StringPart* "\"" 
  ;

lexical HexaSignificand =
  [0] [X x] [0-9 A-F a-f]* "." [0-9 A-F a-f]* 
  | [0] [X x] [0-9 A-F a-f]+ 
  ;

lexical OctaNumeral =
  [0] [0-7]+ 
  ;

lexical HexaNumeral =
  [0] [X x] [0-9 A-F a-f]+ 
  ;

lexical LEX[CharLiteral] =
  "\'" CharContent "\'" 
  ;

lexical EscChar =
  "\\" 
  ;

lexical OctaEscape 
  = "\\" [0-3] [0-7]+ !>> [0-7] 
  | "\\" [0-7] !>> [0-7] 
  | "\\" [4-7] [0-7] 
  ;


lexical EscEscChar =
  "\\\\" 
  ;

lexical DeciNumeral =
  [1-9] [0-9]* 
  | "0" 
  ;

keyword HexaSignificandKeywords =
  [0] [X x] "." 
  ;

lexical StringChars =
  FooStringChars 
  ;

lexical LAYOUT =
  [\t-\n \a0C-\a0D \ ] 
  | Comment 
  ;


lexical CharContent =
  EscapeSeq 
  | UnicodeEscape 
  | SingleChar 
  ;

lexical Comment =
  "/**/" 
  | "//" EOLCommentChars !>> ![\n \a0D] LineTerminator 
  | "/*" !>> [*] CommentPart* "*/" 
  | "/**" !>> [/] CommentPart* "*/" 
  ;

lexical OctaLiteral =
  OctaNumeral !>> [0-7] [L l]? 
  ;

lexical HexaFloatNumeral =
  HexaSignificand \ HexaSignificandKeywords !>> [0-9 A-F a-f] BinaryExponent 
  ;

lexical HexaLiteral =
  HexaNumeral !>> [0-9 A-F a-f] [L l]? 
  ;

lexical DeciFloatLiteral =
  DeciFloatNumeral [D F d f]? 
  ;

lexical ID =
  [$ A-Z _ a-z] [$ 0-9 A-Z _ a-z]* 
  ;
  
lexical DeciFloatDigits =
  [0-9]+ 
  | [0-9]* "." [0-9]* 
  ;

lexical DeciLiteral =
  DeciNumeral !>> [. 0-9 D F d f] [L l]? 
  ;

lexical EscapeSeq =
  NamedEscape 
  | OctaEscape 
  ;

layout LAYOUTLIST  =
  LAYOUT* !>> [\t-\n \a0C-\a0D \ ] !>> (  [/]  [*]  ) !>> (  [/]  [/]  ) !>> "/*" !>> "//"
  ;


lexical NamedEscape =
   "\\" [\" \' \\ b f n r t] 
  ;

lexical BinaryExponent =
  [P p] SignedInteger !>> [0-9] 
  ;

lexical BlockCommentChars =
  ![* \\]+ 
  ;


keyword Keyword =
  "continue" 
  | "package" 
  | "short" 
  | "boolean" 
  | "for" 
  | "extends" 
  | "do" 
  | "strictfp" 
  | "if" 
  | "enum" 
  | "synchronized" 
  | "else" 
  | "interface" 
  | "return" 
  | "private" 
  | "volatile" 
  | "default" 
  | "throws" 
  | "static" 
  | "long" 
  | "throw" 
  | "this" 
  | "catch" 
  | "super" 
  | "const" 
  | "switch" 
  | "int" 
  | "implements" 
  | "native" 
  | "abstract" 
  | "break" 
  | "goto" 
  | "final" 
  | "class" 
  | "byte" 
  | "instanceof" 
  | "void" 
  | "finally" 
  | "try" 
  | "new" 
  | "float" 
  | "public" 
  | "transient" 
  | "char" 
  | "assert" 
  | "case" 
  | "while" 
  | "double" 
  | "protected" 
  | "import" 
  ;

lexical FooStringChars =
  ([\a00] | ![\n \a0D \" \\])+ 
  ;

lexical StringPart =
  UnicodeEscape 
  | EscapeSeq 
  | StringChars !>> ![\n \a0D \" \\]  !>> [\a00]
  ;


keyword FieldAccessKeywords =
  ExprName "." Identifier 
  ;

lexical EOLCommentChars =
  ![\n \a0D]* 
  ;

lexical SingleChar =
  ![\n \a0D \' \\] 
  ;

keyword ElemValKeywords =
  LHS "=" Expr 
  ;

lexical CommentPart =
  UnicodeEscape 
  | BlockCommentChars !>> ![* \\] 
  | EscChar !>> [\\ u] 
  | Asterisk !>> [/] 
  | EscEscChar 
  ;

keyword ArrayAccessKeywords =
  ArrayCreationExpr ArraySubscript 
  ;



lexical DeciFloatExponentPart =
  [E e] SignedInteger !>> [0-9] 
  ;

lexical EndOfFile =
  
  ;

keyword DeciFloatLiteralKeywords =
  [0-9]+ 
  ;

keyword DeciFloatDigitsKeywords =
  "." 
  ;

syntax InstanceInit =
   Block 
  ;

keyword IDKeywords =
  "null" 
  | Keyword 
  | "true" 
  | "false" 
  ;


lexical DeciFloatNumeral
	= [0-9] !<< [0-9]+ DeciFloatExponentPart
	| [0-9] !<< [0-9]+ >> [D F d f]
	| [0-9] !<< [0-9]+ "." [0-9]* !>> [0-9] DeciFloatExponentPart?
	| [0-9] !<< "." [0-9]+ !>> [0-9] DeciFloatExponentPart?
  ;

lexical CarriageReturn =
  [\a0D] 
  ;

lexical UnicodeEscape =
   "\\" [u]+ [0-9 A-F a-f] [0-9 A-F a-f] [0-9 A-F a-f] [0-9 A-F a-f] 
  ;

lexical LineTerminator =
  [\n] 
  | EndOfFile !>> ![] 
  | [\a0D] [\n] 
  | CarriageReturn !>> [\n] 
  ;

lexical HexaFloatLiteral =
  HexaFloatNumeral [D F d f]? 
  ;

lexical Asterisk =
  "*" 
  ;
