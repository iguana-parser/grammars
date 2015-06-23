/**
 * 
 * Derived from the C language specification:
 * 
 * http://www.open-std.org/jtc1/sc22/wg14/www/docs/n1570.pdf
 * 
 * Ali Afroozeh
 */

module c::specification::C

extend c::specification::Lexical;
extend c::specification::Preprocessor;


// External definitions

syntax TranslationUnit 
     = ExternalDeclaration
     | TranslationUnit ExternalDeclaration
     ;

syntax ExternalDeclaration
     = FunctionDefinition
     | Declaration
     ;
     
// DeclarationSpecifiers? added to deal with functions without return value
syntax FunctionDefinition
     = DeclarationSpecifiers? Declarator DeclarationList? CompoundStatement
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
     = Identifier ":" Statement
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
     = "if" "(" Expression ")" Statement !>>> "else"
     | "if" "(" Expression ")" Statement "else" Statement
     | "switch" "(" Expression ")" Statement
     ;

syntax IterationStatement
     = "while" "(" Expression ")" Statement
     | "do" Statement "while" "(" Expression ")" ";"
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

syntax StorageClassSpecifier
     = "typedef"
     | "extern"
     | "static"
     | "_Thread_local"
     | "auto"
     | "register"
     ;

syntax TypeSpecifier
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


// StructDeclarationList? added to support empty structurs as supported by GCC
syntax StructOrUnionSpecifier
     = StructOrUnion Identifier? "{" StructDeclarationList? "}"
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

syntax Pointer
     = "*" TypeQualifierList?
     | "*" TypeQualifierList? Pointer
     ;

syntax TypeQualifierList
     = TypeQualifier
     | TypeQualifierList TypeQualifier
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
     | DirectAbstractDeclarator? "[" TypeQualifierList? AssignmentExpression? "]"
     | DirectAbstractDeclarator? "[" "static" TypeQualifierList? AssignmentExpression "]"
     | DirectAbstractDeclarator? "[" TypeQualifierList "static" AssignmentExpression "]"
     | DirectAbstractDeclarator? "[" "*" "]" DirectAbstractDeclarator? "(" ParameterTypeList? ")"
     ;

syntax TypedefName
     =  Identifier
     ;

// StructDeclarationList? added to support empty structurs as supported by GCC
syntax Initializer
     =  AssignmentExpression
     | "{" InitializerList? "}"
     | "{" InitializerList "," "}"
     ;

syntax InitializerList
     = Designation? Initializer
     | InitializerList "," Designation? Initializer
     ;

syntax Designation
     = DesignatorList "="
     ;

syntax DesignatorList
     = Designator
     | DesignatorList Designator
     ;

syntax Designator
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
     | "(" Expression ")" 
     | GenericSelection
     | "__builtin_offsetof" "(" TypeName "," OffsetOfMemberDesignator ")" // GCC extension to deal with offsetof macro
     ;
     
// GCC extension to deal with offsetof macro
syntax OffsetOfMemberDesignator
     = Identifier
     | OffsetOfMemberDesignator "." Identifier
     | OffsetOfMemberDesignator "[" Expression "]"
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
     | PostfixExpression "-\>" Identifier
     | PostfixExpression "++"
     | PostfixExpression "--"
     | "(" TypeName ")" "{" InitializerList "}"
     | "(" TypeName ")" "{" InitializerList "," "}"
     ;

syntax ArgumentEexpressionList
     = AssignmentExpression
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
     = "&" !>> [&]
     | "*" 
     | "+" !>> [+]
     | "-" !>> [\-]
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
     | AdditiveExpression "+" !>> [+] MultiplicativeExpression
     | AdditiveExpression "-" !>> [\-] MultiplicativeExpression
     ;
     
syntax ShiftExpression
     = AdditiveExpression
     | ShiftExpression "\<\<" AdditiveExpression 
     | ShiftExpression "\>\>" AdditiveExpression
     ;

syntax RelationalExpression
     = ShiftExpression
     | RelationalExpression "\<" !>> [\<] ShiftExpression
     | RelationalExpression "\>" !>> [\>] ShiftExpression 
     | RelationalExpression "\<=" ShiftExpression
     | RelationalExpression "\>=" ShiftExpression
     ;

syntax EqualityExpression
     = RelationalExpression
     | EqualityExpression "==" RelationalExpression 
     | EqualityExpression "!=" RelationalExpression
     ;

syntax AndExpression
     = EqualityExpression
     | AndExpression "&" !>> [&] EqualityExpression
     ;

syntax ExclusiveOrExpression
     = AndExpression
     | ExclusiveOrExpression "^" AndExpression
     ;

syntax InclusiveOrExpression
     = ExclusiveOrExpression
     | InclusiveOrExpression "|" ExclusiveOrExpression
     ;

syntax LogicalAndExpression
     = InclusiveOrExpression
     | LogicalAndExpression "&&" InclusiveOrExpression
     ;

syntax LogicalOrExpression
     = LogicalAndExpression
     | LogicalOrExpression "||" LogicalAndExpression     
     ;

syntax ConditionalExpression
     = LogicalOrExpression
     | LogicalOrExpression "?" Expression ":" ConditionalExpression
     ;

syntax AssignmentExpression
     = ConditionalExpression
     | UnaryExpression AssignmentOperator AssignmentExpression
     ;

syntax AssignmentOperator
     = "=" 
     | "*=" 
     | "/=" 
     | "%=" 
     | "+=" 
     | "-=" 
     | "\<\<=" 
     | "\>\>=" 
     | "&=" 
     | "^=" 
     | "|="
     ;

syntax Expression
     = AssignmentExpression
     | Expression "," AssignmentExpression
     ;

syntax ConstantExpression
     = ConditionalExpression
     ;
