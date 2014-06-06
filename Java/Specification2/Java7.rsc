/**
 *  Derived from http://docs.oracle.com/javase/specs/jls/se7/html/jls-18.html
 *
 *  There are some problems with this grammar that I found solutions for in this
 *  blog post: http://www.daniberg.com/2014/02/java-7-grammar.html
 *
 *  author: Ali Afroozeh
 */
module Java7


syntax QualifiedIdentifier 
     = {Identifier "."}+;

syntax QualifiedIdentifierList 
     = {QualifiedIdentifier  ","}+;

//----------------------------------------------------------------------------------------------------------------

start syntax CompilationUnit 
    = PackageDeclaration? ImportDeclaration* TypeDeclaration*
      ;
  
  
syntax PackageDeclaration 
    = Annotation* "package"  QualifiedIdentifier ";" 
      ;  
  
syntax ImportDeclaration 
    = "import"  "static"?  {Identifier "."}+ ("." "*")? ";" 
    ;
  
syntax TypeDeclaration 
    = ClassOrInterfaceDeclaration 
    | ";" 
      ;  
  
syntax ClassOrInterfaceDeclaration 
    = Modifier* (InterfaceDeclaration | ClassDeclaration)
    ; 
    
syntax ClassDeclaration 
     = NormalClassDeclaration
     | EnumDeclaration
     ;
  
syntax InterfaceDeclaration 
    = NormalInterfaceDeclaration
    | AnnotationTypeDeclaration
    ;
  
syntax NormalClassDeclaration 
    = "class" Identifier TypeParameters? ("extends" Type)? ("implements" TypeList)? ClassBody;
                                
syntax EnumDeclaration 
    = "enum" Identifier ("implements" TypeList)? EnumBody;
                                
syntax NormalInterfaceDeclaration 
    = "interface" Identifier TypeParameters? ("extends" TypeList)? InterfaceBody;

syntax AnnotationTypeDeclaration 
    = "@" "interface" Identifier AnnotationTypeBody;
  
//----------------------------------------------------------------------------------------------------------------  
  
syntax Type 
     = BasicType ("[" "]")* 
     | ReferenceType ("[" "]")*
     ;

syntax BasicType 
     = "byte"
     | "short"
     | "char"
     | "int"
     | "long"
     | "float"
     | "double"
     | "boolean"
     ;
  
syntax ReferenceType 
     = Identifier TypeArguments? ("." Identifier TypeArguments? )*;
  
syntax TypeArguments 
     = "\<" {TypeArgument ","}+ "\>" 
     ;
        
syntax TypeArgument  // fix: changed ReferenceType to Type to deal with primitive array types such as < String[] > 
     = Type
     | "?" (("extends" | "super") Type)?  
     ;
  
//----------------------------------------------------------------------------------------------------------------    
  
syntax NonWildcardTypeArguments 
     =  "\<" {Type ","}+ "\>"  // fix: changed ReferenceType to Type to deal with primitive array types such as < String[] >  
     ;

syntax TypeList
     = {ReferenceType ","}+    
     ;
    
syntax TypeArgumentsOrDiamond 
     = "\<" "\>" 
     | TypeArguments
     ;

syntax NonWildcardTypeArgumentsOrDiamond 
     = "\<" "\>" 
     | NonWildcardTypeArguments
     ;

syntax TypeParameters 
     = "\<" {TypeParameter ","}+ "\>"
     ;

syntax TypeParameter 
     = Identifier ("extends" Bound)?
     ;

syntax Bound 
     = {ReferenceType "&"}+
     ;  
    
//----------------------------------------------------------------------------------------------------------------    

syntax Modifier 
     = Annotation
     | "final"
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

  
syntax Annotation 
    = "@" QualifiedIdentifier  ( "(" AnnotationElement? ")" )?
      ;

syntax AnnotationElement 
    = ElementValuePairs
    | ElementValue
    ;

syntax ElementValuePairs 
    = {ElementValuePair ","}+
    ;

syntax ElementValuePair 
    = Identifier "=" ElementValue
    ;

syntax ElementValue 
    = Annotation
    | Expression
    | ElementValueArrayInitializer
    ;

syntax ElementValueArrayInitializer 
    = "{" ElementValues? ","? "}"
    ;

syntax ElementValues 
    = {ElementValue ","}+
    ;

//----------------------------------------------------------------------------------------------------------------

syntax ClassBody 
    = "{" ClassBodyDeclaration* "}"
        ;

syntax ClassBodyDeclaration 
    =  ";" 
    | Modifier* MemberDecl
    | "static"? Block
    ;

syntax MemberDecl 
    = MethodOrFieldDecl
    | "void" Identifier VoidMethodDeclaratorRest
    | Identifier ConstructorDeclaratorRest
    | GenericMethodOrConstructorDecl
    | ClassDeclaration
    | InterfaceDeclaration
    ;

syntax MethodOrFieldDecl 
    = Type Identifier MethodOrFieldRest
    ;

syntax MethodOrFieldRest 
    =  FieldDeclaratorsRest ";"
    |  MethodDeclaratorRest
    ;

syntax FieldDeclaratorsRest 
    =  VariableDeclaratorRest ("," VariableDeclarator)*
    ;

syntax MethodDeclaratorRest 
    = FormalParameters ("[" "]")* ("throws" QualifiedIdentifierList)? (Block | ";")
    ;

syntax VoidMethodDeclaratorRest 
    = FormalParameters ("throws" QualifiedIdentifierList)? (Block | ";")
    ;

syntax ConstructorDeclaratorRest 
    = FormalParameters ("throws" QualifiedIdentifierList)? Block
    ;

syntax GenericMethodOrConstructorDecl 
    = TypeParameters GenericMethodOrConstructorRest
    ;

syntax GenericMethodOrConstructorRest 
    = (Type | "void") Identifier MethodDeclaratorRest
    | Identifier ConstructorDeclaratorRest
    ;
    
//----------------------------------------------------------------------------------------------------------------

syntax InterfaceBody 
    = "{" InterfaceBodyDeclaration* "}"
    ;

syntax InterfaceBodyDeclaration 
    = ";" 
    | Modifier* InterfaceMemberDecl
    ;

syntax InterfaceMemberDecl 
    = InterfaceMethodOrFieldDecl
    | "void" Identifier VoidInterfaceMethodDeclaratorRest
    | InterfaceGenericMethodDecl
    | ClassDeclaration
    | InterfaceDeclaration
    ;

syntax InterfaceMethodOrFieldDecl 
    = Type Identifier InterfaceMethodOrFieldRest
    ;

syntax InterfaceMethodOrFieldRest 
    = ConstantDeclaratorsRest ";"
    | InterfaceMethodDeclaratorRest
    ;

syntax ConstantDeclaratorsRest 
    = ConstantDeclaratorRest ("," ConstantDeclarator)*
    ;

syntax ConstantDeclaratorRest 
    = ("[" "]")* "=" VariableInitializer
    ;

syntax ConstantDeclarator 
    = Identifier ConstantDeclaratorRest
    ;

syntax InterfaceMethodDeclaratorRest 
    = FormalParameters ("[" "]")* ("throws" QualifiedIdentifierList)? ";"
    ; 

syntax VoidInterfaceMethodDeclaratorRest 
    = FormalParameters ("throws" QualifiedIdentifierList)? ";"
    ;  

syntax InterfaceGenericMethodDecl 
    = TypeParameters (Type | "void") Identifier InterfaceMethodDeclaratorRest
    ;

//----------------------------------------------------------------------------------------------------------------

syntax FormalParameters 
    = "(" FormalParameterDecls? ")"
    ;

syntax FormalParameterDecls 
    = VariableModifier*  Type FormalParameterDeclsRest
    ;

syntax VariableModifier 
    = "final"
    | Annotation
    ;

syntax FormalParameterDeclsRest 
    = VariableDeclaratorId ("," FormalParameterDecls)?
    | "..." VariableDeclaratorId
    ;

syntax VariableDeclaratorId 
    = Identifier ("[" "]")*
    ;

syntax VariableDeclarators 
    = {VariableDeclarator ","}+
    ;

syntax VariableDeclarator 
    = Identifier VariableDeclaratorRest
    ;

syntax VariableDeclaratorRest 
    = ("[" "]")* ("=" VariableInitializer)?
    ;

syntax VariableInitializer 
    = ArrayInitializer
    | Expression
    ;

syntax ArrayInitializer 
    = "{" ({VariableInitializer ","}+ ","? )? "}"
    ;

//----------------------------------------------------------------------------------------------------------------

syntax Block 
    = "{" BlockStatement* "}"
    ;

syntax BlockStatement 
    = LocalVariableDeclarationStatement
    | ClassOrInterfaceDeclaration
    | (Identifier ":")? Statement
    ;

syntax LocalVariableDeclarationStatement 
    = VariableModifier*  Type VariableDeclarators ";"
    ;

syntax Statement 
    = Block
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
    | "return" Expression? ";"
    | "throw" Expression ";"
    | "synchronized" ParExpression Block
    | "try" Block (CatchClause+ | (CatchClause* Finally))
    | "try" ResourceSpecification Block CatchClause* Finally?
    ;

syntax StatementExpression 
    =  Expression
    ;
    
//----------------------------------------------------------------------------------------------------------------

syntax CatchClause 
    = "catch" "(" VariableModifier* CatchType Identifier ")" Block
    ;

syntax CatchType  
    =  {QualifiedIdentifier "|"}+
    ;

syntax Finally 
    = "finally" Block
    ;

syntax ResourceSpecification 
    = "(" Resources ";"? ")"
    ;

syntax Resources 
    = {Resource ";"}+
    ;

syntax Resource 
    = VariableModifier* ReferenceType VariableDeclaratorId "=" Expression
    ; 

//----------------------------------------------------------------------------------------------------------------

syntax SwitchBlockStatementGroups 
    = SwitchBlockStatementGroup*
    ;

syntax SwitchBlockStatementGroup =  
    SwitchLabel+ BlockStatement*
    ;

syntax SwitchLabel 
    = "case" Expression ":"
    | "case" EnumConstantName ":"
    | "default" ":"
    ;

syntax EnumConstantName 
    = Identifier
    ;

syntax ForControl 
    = ForVarControl
    | ForInit? ";" Expression? ";" ForUpdate?  // fix: ForInit changed to ForInit? to deal with for (;;;)
    ;

syntax ForVarControl 
    = VariableModifier* Type VariableDeclaratorId  ForVarControlRest
    ;

syntax ForVarControlRest 
    = ForVariableDeclaratorsRest ";" Expression? ";" ForUpdate?
    | ":" Expression
    ;

syntax ForVariableDeclaratorsRest 
    = ("=" VariableInitializer)? ( "," VariableDeclarator)*
    ;

syntax ForInit 
     = {StatementExpression ","}+;

syntax ForUpdate 
     = {StatementExpression ","}+
     ;    

//----------------------------------------------------------------------------------------------------------------

syntax Primary
	 =  PrimaryNoNewArray 
	 |  ArrayCreationExpression
	 ;
	 

syntax PrimaryNoNewArray 
     = Literal
     | Type "." "class"
     | "void" "." "class"
     | "this" 
     | ClassName "." "this"   
     | "(" Expression ")" 
     | ClassInstanceCreationExpression 
     | FieldAccess
     | MethodInvocation 
     | ArrayAccess
     ;

syntax ClassInstanceCreationExpression
     = "new" TypeArguments? TypeDeclSpecifier TypeArgumentsOrDiamond?  "(" ArgumentList? ")" ClassBody? 
     | Primary "." "new" TypeArguments? Identifier TypeArgumentsOrDiamond? "(" ArgumentList? ")" ClassBody? 
     ;
     
syntax ArgumentList
     = {Expression ","}+
     ;     

syntax ArrayCreationExpression
	 = "new" BasicType DimExpr+ Dims?
	 | "new" ReferenceType DimExpr+ Dims? 
	 | "new" BasicType Dims ArrayInitializer
     | "new" ReferenceType Dims ArrayInitializer
     ;
     
syntax DimExpr
     = "[" Expression "]"
     ;
     
syntax Dims
	 = ("[" "]")+
	 ;


syntax FieldAccess
     = Primary "." Identifier
     | "super" "." Identifier
     | ClassName "." "super" "." Identifier
     ;
     
syntax MethodInvocation
     = MethodName "(" ArgumentList? ")"
     | Primary "." NonWildTypeArguments? Identifier "(" ArgumentList? ")"
     | "super" "." NonWildTypeArguments? Identifier "(" ArgumentList? ")"
     | ClassName "." "super" "." NonWildTypeArguments? Identifier "(" ArgumentList? ")"
     | TypeName "." NonWildTypeArguments Identifier "(" ArgumentList? ")"
     ;
     
syntax NonWildTypeArguments
     = {ReferenceType ","}+
     ;     
     
syntax ArrayAccess
     = ExpressionName "[" Expression "]" 
     | PrimaryNoNewArray "[" Expression "]"
     ;
 
syntax PostfixExpression
     = Primary
     | ExpressionName 
     | PostIncrementExpression 
     | PostDecrementExpression
     ;

syntax PostIncrementExpression
     = PostfixExpression "++"
     ;

syntax PostDecrementExpression
     = PostfixExpression "--"
     ;
     
syntax UnaryExpression
     = PreIncrementExpression 
     | PreDecrementExpression 
     | "+" UnaryExpression
     | "-" UnaryExpression
     | UnaryExpressionNotPlusMinus
     ;
     
     
syntax PreIncrementExpression
     = "++" UnaryExpression
     ;
     
     
syntax PreDecrementExpression
     = "--" UnaryExpression
     ;
     
syntax UnaryExpressionNotPlusMinus
     = PostfixExpression
     | "~" UnaryExpression
     | "!" UnaryExpression 
     | CastExpression
     ;

syntax CastExpression
     = "(" BasicType ")" UnaryExpression
     | "(" ReferenceType ")" UnaryExpressionNotPlusMinus
     ;
     
syntax MultiplicativeExpression
     = UnaryExpression
     | MultiplicativeExpression "*" UnaryExpression 
     | MultiplicativeExpression "/" UnaryExpression
     | MultiplicativeExpression "%" UnaryExpression
     ;
     
syntax AdditiveExpression
     = MultiplicativeExpression
     | AdditiveExpression "+" MultiplicativeExpression
     | AdditiveExpression "-" MultiplicativeExpression     
     ;
     
syntax ShiftExpression
     = AdditiveExpression
     | ShiftExpression "\<\<" AdditiveExpression
     | ShiftExpression "\>\>" AdditiveExpression
     | ShiftExpression "\>\>\>" AdditiveExpression
     ;
     

syntax RelationalExpression
     = ShiftExpression
     | RelationalExpression "\<" ShiftExpression 
     | RelationalExpression "\>" ShiftExpression 
     | RelationalExpression "\<=" ShiftExpression 
     | RelationalExpression "\>=" ShiftExpression 
     | RelationalExpression "instanceof" ReferenceType
     ;


syntax EqualityExpression
     = RelationalExpression
     | EqualityExpression "==" RelationalExpression
     | EqualityExpression "!=" RelationalExpression
     ;
     
syntax AndExpression
     = EqualityExpression
     | AndExpression "&" EqualityExpression
     ;
     
syntax ExclusiveOrExpression
     = AndExpression
     | ExclusiveOrExpression "^" AndExpression
     ;
     
syntax InclusiveOrExpression
     = ExclusiveOrExpression
     | InclusiveOrExpression "|" ExclusiveOrExpression
     ;
     
syntax ConditionalAndExpression
     = InclusiveOrExpression
     | ConditionalAndExpression "&&" InclusiveOrExpression
     ;
     
syntax ConditionalOrExpression
     = ConditionalAndExpression
     | ConditionalOrExpression "||" ConditionalAndExpression
     ;
     

syntax ConditionalExpression
     = ConditionalOrExpression
     | ConditionalOrExpression "?" Expression ":" ConditionalExpression
     ;
     
syntax AssignmentExpression
     = ConditionalExpression 
     | Assignment
     ;
     
     
syntax Assignment
     = LeftHandSide AssignmentOperator AssignmentExpression
     ;
     
syntax LeftHandSide
     = ExpressionName 
     | FieldAccess 
     | ArrayAccess
     ;

syntax AssignmentOperator 
    = "=" 
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

syntax Expression
     = AssignmentExpression
     ;
     
syntax ConstantExpression
     = Expression
     ;
     
syntax ClassName
	 = QualifiedIdentifier
	 ;
	 
syntax TypeName
     = QualifiedIdentifier
     ;
     
syntax ExpressionName
     = QualifiedIdentifier
     ;
     
syntax MethodName
     = QualifiedIdentifier
     ;
     
syntax TypeName
     = QualifiedIdentifier
     ;     

syntax ParExpression 
     =  "(" Expression ")"
     ;

syntax Arguments 
     = "(" {Expression ","}* ")"
     ;

syntax TypeDeclSpecifier
     = TypeName
     | ReferenceType "." Identifier
     ;     

syntax SuperSuffix 
     =  Arguments 
     | "." Identifier Arguments?
     ;

syntax ExplicitGenericInvocationSuffix 
     = "super" SuperSuffix
     | Identifier Arguments
     ;
    
//----------------------------------------------------------------------------------------------------------------

syntax Creator 
     = NonWildcardTypeArguments CreatedName ClassCreatorRest
     | CreatedName (ClassCreatorRest | ArrayCreatorRest)
     | BasicType ArrayCreatorRest   // fix: to deal with primitive array types such as new int[1]
     ;

syntax CreatedName 
     = Identifier TypeArgumentsOrDiamond? ("." Identifier TypeArgumentsOrDiamond?)*
     ;

syntax ClassCreatorRest 
     = Arguments ClassBody?
     ;

syntax ArrayCreatorRest 
     = "[" ( ("]" ("[" "]")* ArrayInitializer)  |  (Expression "]" ("[" Expression "]")* ("[" "]")*) )
     ;


syntax IdentifierSuffix 
     = "[" ( ("]"  ("[" "]")* "." "class") | Expression) "]"  // fix: added the second "]"
     | Arguments 
     | "." ("class" | ExplicitGenericInvocation | "this" | ("super" Arguments) |("new" NonWildcardTypeArguments? InnerCreator))
     ;

syntax ExplicitGenericInvocation 
     = NonWildcardTypeArguments ExplicitGenericInvocationSuffix
     ;

syntax InnerCreator 
     =  Identifier NonWildcardTypeArgumentsOrDiamond? ClassCreatorRest
     ;

syntax Selector 
     = "." Identifier Arguments?
     | "." ExplicitGenericInvocation
     | "." "this"
     | "." "super" SuperSuffix
     | "." "new" NonWildcardTypeArguments? InnerCreator
     | "[" Expression "]"
     ;

//----------------------------------------------------------------------------------------------------------------
syntax EnumBody 
     = "{" {EnumConstant ","}* ","? EnumBodyDeclarations? "}"
     ;

syntax EnumConstant 
     = Annotation* Identifier Arguments? ClassBody?;

syntax EnumBodyDeclarations 
     = ";" ClassBodyDeclaration*
     ;

syntax AnnotationTypeBody 
     = "{" AnnotationTypeElementDeclaration+ "}"
     ;


syntax AnnotationTypeElementDeclaration 
    = Modifier* AnnotationTypeElementRest
    ;

syntax AnnotationTypeElementRest 
    = Type Identifier AnnotationMethodOrConstantRest ";"
    | ClassDeclaration
    | InterfaceDeclaration
    | EnumDeclaration  
    | AnnotationTypeDeclaration
    ;

syntax AnnotationMethodOrConstantRest 
    = AnnotationMethodRest
    | ConstantDeclaratorsRest
    ;  

syntax AnnotationMethodRest 
    = "(" ")" ("[""]")? ("default" ElementValue)?
    ;

//----------------------------------------------------------------------------------------------------------------
// Lexical Definititions
//----------------------------------------------------------------------------------------------------------------

lexical UnicodeInputCharacter 
     = UnicodeEscape
     | RawInputCharacter
     ;

lexical UnicodeEscape 
     = [\\] UnicodeMarker HexDigit HexDigit HexDigit HexDigit
     ;
 
lexical UnicodeMarker 
     = [u]
     | UnicodeMarker [u]
     ;

lexical RawInputCharacter = [0-0x10FFFF];

lexical HexDigit = [0-9 a-f A-F];

//----------------------------------------------------------------------------------------------------------------
lexical LineTerminator
	 = [\n]         // the ASCII LF character, also known as "newline"
     | [\r]         //the ASCII CR character, also known as "return"
     | [\r][\n]  //the ASCII CR character followed by the ASCII LF character
     ;

lexical InputCharacter 
	 = ![\n \r]   //UnicodeInputCharacter but not CR or LF // [\a00] to match zero
     ;
//----------------------------------------------------------------------------------------------------------------

lexical Input 
     = InputElement* Sub?
     ;

lexical InputElement 
     = WhiteSpace
     | Comment
     | Token
     ;

lexical Token 
     = Identifier
     | Keyword
     | Literal
     | Separator
     | Operator
     ;

lexical Sub 
    = [\u001A]        //the ASCII SUB character, also known as "control-Z"
    ;

//----------------------------------------------------------------------------------------------------------------

layout Layout 
    = (WhiteSpace | Comment)* !>> [\t \n \r \f  \ ] !>> "/*" !>> "//";

lexical WhiteSpace 
    = [\ ]            // the ASCII SP character, also known as "space"
    | [\t]            // the ASCII HT character, also known as "horizontal tab"
    | [\f]            // the ASCII FF character, also known as "form feed"
    | LineTerminator
    ;
    
//----------------------------------------------------------------------------------------------------------------
    
lexical Comment 
    = TraditionalComment
    | EndOfLineComment
    ;

lexical TraditionalComment 
     = "/*" CommentTail
     ;

lexical EndOfLineComment 
    = "//" InputCharacter* !>> ![\n \r];

lexical CommentTail 
    = "*" CommentTailStar
    | NotStar CommentTail
    ;

lexical CommentTailStar 
    = "/"
    | "*" CommentTailStar
    | NotStarNotSlash CommentTail
    ;

lexical NotStar 
    = InputCharacter \ [*]
    | LineTerminator;

lexical NotStarNotSlash 
    = InputCharacter \ [* /]
    | LineTerminator;

    
//----------------------------------------------------------------------------------------------------------------      

lexical Identifier = [$ A-Z _ a-z] !<< IdentifierChars \Keyword \BooleanLiteral \NullLiteral !>> [$ 0-9 A-Z _ a-z];

lexical IdentifierChars 
    = JavaLetter
    | IdentifierChars JavaLetterOrDigit
    ;

lexical JavaLetter = [A-Za-z$_];

lexical JavaLetterOrDigit = [A-Za-z$_0-9];

//----------------------------------------------------------------------------------------------------------------      

keyword Keyword 
      = "abstract"
      | "continue"
      | "for"
      | "new"
      | "switch"
      | "assert"
      | "default"
      | "if"
      | "package"
      | "synchronized"
      | "boolean"
      | "do"
      | "goto"
      | "private"
      | "this"
      | "break"
      | "double"
      | "implements"
      | "protected"
      | "throw"
      | "byte"
      | "else"
      | "import"
      | "public"
      | "throws"
      | "case"
      | "enum"
      | "instanceof"
      | "return"
      | "transient"
      | "catch"
      | "extends"
      | "int"
      | "short"
      | "try"
      | "char"
      | "final"
      | "interface"
      | "static"
      | "void"
      | "class"
      | "finally"
      | "long"
      | "strictfp"
      | "volatile"
      | "const"
      | "float"
      | "native"
      | "super"
      | "while"
      ;

//----------------------------------------------------------------------------------------------------------------

lexical Literal 
    = IntegerLiteral
    | FloatingPointLiteral
    | BooleanLiteral
    | CharacterLiteral
    | StringLiteral
    | NullLiteral
    ;
          
//----------------------------------------------------------------------------------------------------------------

lexical IntegerLiteral 
    = DecimalIntegerLiteral
    | HexIntegerLiteral    
    | OctalIntegerLiteral
    | BinaryIntegerLiteral
    ; 

lexical DecimalIntegerLiteral 
    = DecimalNumeral IntegerTypeSuffix?
    ;

lexical HexIntegerLiteral 
    = HexNumeral IntegerTypeSuffix?
    ;

lexical OctalIntegerLiteral 
    = OctalNumeral IntegerTypeSuffix?
    ;

lexical BinaryIntegerLiteral 
    = BinaryNumeral IntegerTypeSuffix?
    ;

lexical IntegerTypeSuffix 
    = [l] | [L];
    
//----------------------------------------------------------------------------------------------------------------
    
lexical DecimalNumeral 
    = [0]
    | NonZeroDigit Digits?
    | NonZeroDigit [_]+ Digits
    ; 

lexical Digits 
    = Digit
    | Digit DigitOrUnderscore* Digit
    ; 

lexical Digit 
     = [0]
     | NonZeroDigit
     ;

lexical NonZeroDigit = [1-9];

lexical DigitOrUnderscore 
    = Digit
    | [_]
    ;

//----------------------------------------------------------------------------------------------------------------

lexical HexNumeral 
    = [0] [x] HexDigits
    | [0] [X] HexDigits
    ;

lexical HexDigits 
    = HexDigit
    | HexDigit HexDigitOrUnderscore* HexDigit; 

lexical HexDigit = [0-9 a-f A-F];

lexical HexDigitOrUnderscore 
    = HexDigit
    | [_]
    ;
    
//----------------------------------------------------------------------------------------------------------------    
    
lexical OctalNumeral 
    = [0] OctalDigits
    | [0] [_]+ OctalDigits
    ;

lexical OctalDigits 
    = OctalDigit
    | OctalDigit OctalDigitOrUnderscore* OctalDigit 
    ;

lexical OctalDigit = [0-7];

lexical OctalDigitOrUnderscore 
    = OctalDigit
    | [_]
    ;
    
//----------------------------------------------------------------------------------------------------------------        
    
lexical BinaryNumeral 
    = [0] [b] BinaryDigits 
    | [0] [B] BinaryDigits
    ;

lexical BinaryDigits 
    = BinaryDigit 
    | BinaryDigit BinaryDigitOrUnderscore* BinaryDigit
    ;

lexical BinaryDigit = [0-1]; 

lexical BinaryDigitOrUnderscore
    = BinaryDigit
    | [_]
    ;
    
//----------------------------------------------------------------------------------------------------------------        

lexical FloatingPointLiteral 
    = DecimalFloatingPointLiteral
    | HexadecimalFloatingPointLiteral
    ;

lexical DecimalFloatingPointLiteral 
    = Digits [.] Digits ExponentPart? FloatTypeSuffix?
    | [.] Digits ExponentPart? FloatTypeSuffix?
    | Digits ExponentPart FloatTypeSuffix?
    | Digits ExponentPart? FloatTypeSuffix
    ;

lexical ExponentPart 
    = ExponentIndicator SignedInteger
    ;

lexical ExponentIndicator = [e E];

lexical SignedInteger = Sign? Digits;

lexical Sign = [+ \-];

lexical FloatTypeSuffix = [f F d D];     
    
//----------------------------------------------------------------------------------------------------------------

lexical HexadecimalFloatingPointLiteral 
      =  HexSignificand BinaryExponent FloatTypeSuffix;

lexical HexSignificand 
    = HexNumeral
    | HexNumeral [.]
    | [0] [x] HexDigits? [.] HexDigits
    | [0] [X] HexDigits? [.] HexDigits
    ;

lexical BinaryExponent 
    = BinaryExponentIndicator SignedInteger;

lexical BinaryExponentIndicator = [p P];

//----------------------------------------------------------------------------------------------------------------

lexical BooleanLiteral 
     = "true" 
     | "false"
     ;

lexical CharacterLiteral 
    = [\'] SingleCharacter [\']
    | [\'] EscapeSequence [\']
    ;

lexical SingleCharacter 
      = InputCharacter \ [\' \\]
      ;

lexical StringLiteral 
    = [\"] StringCharacter* [\"]
    ;

lexical StringCharacter 
    = InputCharacter \ [\" \\]
    | EscapeSequence
    ;
        
lexical EscapeSequence 
    = [\\] [b]                 /* \u0008: backspace BS */
    | [\\] [t]                /* \u0009: horizontal tab HT */
    | [\\] [n]               /* \u000a: linefeed LF */
    | [\\] [f]                /* \u000c: form feed FF */
    | [\\] [r]                /* \u000d: carriage return CR */
    | [\\] [\"]                /* \u0022: double quote " */
    | [\\] [\']               /* \u0027: single quote ' */
    | [\\] [\\]             /* \u005c: backslash \ */
    | OctalEscape            /* \u0000 to \u00ff: from octal value */
    ;

lexical OctalEscape 
    = [\\] OctalDigit
    | [\\] OctalDigit OctalDigit
    | [\\] ZeroToThree OctalDigit OctalDigit
    ;

lexical OctalDigit = [0-7];

lexical ZeroToThree = [0-3];
    
lexical NullLiteral = "null";

lexical Separator = [( ) { } \[ \] ; , .];
   
lexical Operator 
     = "=" 
     | "\>" 
     | "\<" 
     | "!" 
     | "~" 
     | "?" 
     | ":" 
     | "==" 
     | "\<=" 
     | "\>=" 
     | "!=" 
     | "&&" 
     | "||" 
     | "++" 
     | "--" 
     | "+" 
     | "-" 
     | "*" 
     | "/" 
     | "&" 
     | "|" 
     | "^" 
     | "%" 
     | "\<\<" 
     | "\>\>" 
     | "\>\>\>" 
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
    