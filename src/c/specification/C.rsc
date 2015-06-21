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



























