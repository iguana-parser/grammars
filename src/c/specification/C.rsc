/**
 * 
 * Derived from the C language specification:
 * 
 * http://www.open-std.org/jtc1/sc22/wg14/www/docs/n1570.pdf
 * 
 * Ali Afroozeh
 */

// External definitions

syntax TranslationUnit 
     = ExternalDeclaration
     | TranslationUnit ExternalDeclaration
     ;

syntax ExternalDeclaration
     = FunctionDefinition
     | Declaration
     ;

syntax FunctionDefinition
     = DeclarationSpecifiers Declarator DeclarationList? CompoundStatement
     ;

syntax DeclarationList
     = Declaration
     | DeclarationList Declaration
     ;

// Statements

syntax Statement
     = LabeledStatement
     | CompoundStatement
     | ExpressionStatement
     | SelectionStatement
     | IterationStatement
     | JumpStatement
     ;

syntax LabeledStatement
     = Identifier ":" statement
     | "case" ConstantExpression ":" Statement
     | "default" ":" Statement
     ;

syntax CompoundStatement
     = "{" BlockItemList? "}"
     ;
     
syntax BlockItemList
     = BlockItem
     | BlockItemList BlockItem
     ;

syntax BlockItem
     = Declaration
     | Statement
     ;

syntax ExpressionStatement
     = Expression? ";"
     ;

syntax SelectionStatement
     = "if" "(" Expression ")" Statement
     | "if" "(" Expression ")" Statement "else" Statement
     | "switch" "(" Expression ")" Statement
     ;

syntax IterationStatement
     = "while" "(" Expression ")" Statement
     | "do" statement while ( expression ) ;
     | "for" "(" Expression? ";" Expression? ";" Expression? ")" Statement
     | "for" "(" Declaration Expression? ";" Expression? ")" Statement
     ;

syntax JumpStatement
     = "goto" Identifier ";"
     | "continue" ";"
     | "break" ";"
     | "return" Expression? ";"
     ;

// Declarations

syntax Declaration
     = DeclarationSpecifiers InitDeclaratorList? ";"
     | StaticAssertDeclaration
     ;

syntax DeclarationSpecifiers
     = StorageClassSpecifier DeclarationSpecifiers?
     | TypeSpecifier DeclarationSpecifiers?
     | TypeQualifier DeclarationSpecifiers?
     | FunctionSpecifier DeclarationSpecifiers?
     | AlignmentSpecifier DeclarationSpecifiers?
     ;

syntax InitDeclaratorList
     = InitDeclarator
     | InitDeclaratorList "," InitDeclarator
     ;

syntax InitDeclarator
     = Declarator
     | Declarator "=" Initializer
     ;

syntax StorageClassSpecifier:
     = "typedef"
     | "extern"
     | "static"
     | "_Thread_local"
     | "auto"
     | "register"
     ;

syntax type-specifier
     = "void"
     | "char"
     | "short"
     | "int"
     | "long"
     | "float"
     | "double"
     | "signed"
     | "unsigned"
     | "_Bool"
     | "_Complex"
     | AtomicTypeSpecifier
     | StructOrUnionSpecifier
     | EnumSpecifier
     | TypedefName
     ;

syntax StructOrUnionSpecifier
     = StructOrUnion Identifier? "{" StructDeclarationList "}"
     | StructOrUnion Identifier
     ;

syntax StructOrUnion
     = "struct"
     | "union"
     ;

syntax StructDeclarationList
     = StructDeclaration
     | StructDeclarationList StructDeclaration
     ;

syntax StructDeclaration
     = SpecifierQualifierList StructDeclaratorList? ";"
     | StaticAssertDeclaration
     ;

syntax SpecifierQualifierList
     = TypeSpecifier SpecifierQualifierList?
     | TypeQualifier SpecifierQualifierList?
     ;

syntax StructDeclaratorList
     = StructDeclarator
     | StructDeclaratorList "," StructDeclarator
     ;

syntax StructDeclarator
     = Declarator
     | Declarator? ":" ConstantExpression
     ; 

syntax EnumSpecifier
     = "enum" Identifier? "{" EnumeratorList "}"
     | "enum" Identifier? "{" EnumeratorList "," "}"
     | "enum" Identifier
     ;

syntax EnumeratorList
     = Enumerator
     | EnumeratorList "," Enumerator
     ;

syntax Enumerator
     = EnumerationConstant
     | EnumerationConstant "=" ConstantExpression
     ;

syntax AtomicTypeSpecifier
     = "_Atomic" "(" TypeName ")"
     ;

syntax TypeQualifier
     = "const"
     | "restrict"
     | "volatile"
     | "_Atomic"
     ;

syntax FunctionSpecifier
     = "inline"
     | "_Noreturn"
     ;

syntax AlignmentSpecifier
     = "_Alignas" "(" TypeName ")"
     | "_Alignas" "(" ConstantExpression ")"
     ;

syntax Declarator
     = Pointer? DirectDeclarator
     ;

syntax DirectDeclarator
     = Identifier
     | "(" Declarator ")"
     | DirectDeclarator "[" TypeQualifierList? AssignmentExpression? "]"
	 | DirectDeclarator "[" "static" TypeQualifierList? AssignmentExpression "]"
     | DirectDeclarator "[" TypeQualifierList "static" AssignmentExpression "]"
     | DirectDeclarator "[" TypeQualifierList? "*" "]"
     | DirectDeclarator "(" ParameterTypeList ")"
     | DirectDeclarator "(" IdentifierList? ")"
     ;

syntax pointer
     = "*" TypeQualifierList?
     | "*" TypeQualifierList? Pointer
     ;

syntax TypeQualifierList
     = TypeQualifier
     | TypeQualifierList typeQualifier
     ;

syntax ParameterTypeList
     = ParameterList
     | ParameterList "," "..."
     ;

syntax ParameterList
     = ParameterDeclaration
     | ParameterList "," ParameterDeclaration
     ;

syntax ParameterDeclaration
     = DeclarationSpecifiers Declarator
     | DeclarationSpecifiers AbstractDeclarator?
     ;

syntax IdentifierList
     = Identifier
     | IdentifierList "," Identifier
     ;

syntax TypeName
     = SpecifierQualifierList AbstractDeclarator?
     ;

syntax AbstractDeclarator
     = Pointer
     | Pointer? DirectAbstractDeclarator
     ;

syntax DirectAbstractDeclarator
     = "(" AbstractDeclarator ")"
     | DirectAbstractDeclarator? "[" TypeQqualifierList? AssignmentExpression? "]"
     | DirectAbstractDeclarator? "[" "static" TypeQqualifierList? AssignmentExpression "]"
     | DirectAbstractDeclarator? "[" TypeQualifierList "static" AssignmentExpression "]"
     | DirectAbstractDeclarator? "[" "*" "]" DirectAbstractDeclarator? "(" ParameterTypeList? ")"
     ;

syntax TypedefName
     =  Identifier
     ;

syntax Initializer
     =  AssignmentExpression
     | "{" InitializerList "}"
     | "{" InitializerList "," "}"
     ;

syntax InitializerList
     = Designation? Initializer
     | InitializerList "," Designation? Initializer 

syntax Designation
     = DesignatorList "="
     ;

syntax DesignatorList
     = Designator
     | DesignatorList Designator
     ;

syntax designator
     = "[" ConstantExpression "]"
     | "." Identifier 
     ;

syntax StaticAssertDeclaration
     = "_Static_assert" "(" ConstantExpression "," StringLiteral ")" ";"
     ;

// Expressions

syntax PrimaryExpression
     = Identifier
     | Constant
     | StringLiteral
     | "(" Expression ")" GenericSelection
     ;

syntax GenericSelection
     = "_Generic" "(" AssignmentExpression "," GenericAssocList ")"
     ;

syntax GenericAssocList
     = GenericAssociation
     | GenericAssocList "," GenericAssociation
     ;

syntax GenericAssociation
     = TypeName ":" AssignmentExpression
     | "default" ":" AssignmentExpression
     ;

syntax PostfixExpression
     = PrimaryExpression
     | PostfixExpression "[" Expression "]"
     | PostfixExpression "(" ArgumentEexpressionList? ")" 
     | PostfixExpression "." Identifier
     | PostfixExpression "->" Identifier
     | PostfixExpression "++"
     | PostfixExpression "--"
     | "(" TypeName ")" "{" InitializerList "}"
     | "(" TypeName ")" "{" InitializerList "," "}"
     ;

syntax ArgumentEexpressionList
     =  AssignmentExpression
     | ArgumentEexpressionList "," AssignmentExpression
     ;

syntax UnaryExpression
    = PostfixExpression
    | "++" UnaryExpression
    | "--" UnaryExpression 
    | UnaryOperator CastExpression 
    | "sizeof" UnaryExpression 
    | "sizeof" "(" TypeName ")"
    | "_Alignof" "(" TypeName ")"
    ;

syntax UnaryOperator
     =  "&"
     |  "*"
     | "+"
     | "-"
     | "~"
     | "!"
     ;

syntax CastExpression
     = UnaryExpression
     | "(" TypeName ")" CastExpression
     ;

syntax MultiplicativeExpression
     = CastExpression
     | MultiplicativeExpression "*" CastExpression 
     | MultiplicativeExpression "/" CastExpression 
     | MultiplicativeExpression "%" CastExpression
     ;

syntax AdditiveExpression
     = MultiplicativeExpression
     | AdditiveExpression "+" MultiplicativeExpression
     | AdditiveExpression "-" MultiplicativeExpression
     ;
     
(6.5.7) shift-expression: AdditiveExpression
shift-expression << AdditiveExpression shift-expression >> AdditiveExpression
(6.5.8) relational-expression: shift-expression
relational-expression relational-expression relational-expression relational-expression
(6.5.9) equality-expression: relational-expression
< shift-expression > shift-expression <= shift-expression >= shift-expression
equality-expression equality-expression
(6.5.10) AND-expression: equality-expression
AND-expression &
(6.5.11) exclusive-OR-expression: AND-expression
== relational-expression != relational-expression
equality-expression
exclusive-OR-expression ^ AND-expression






















