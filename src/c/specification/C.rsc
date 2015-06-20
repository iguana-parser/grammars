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




