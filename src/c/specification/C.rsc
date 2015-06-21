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
     
(6.8.2) block-item-list:
block-item
block-item-list block-item
(6.8.2) block-item:
declaration
statement
(6.8.3) expression-statement:
expressionopt ;
(6.8.4) selection-statement:
if ( expression ) statement
if ( expression ) statement else statement
switch ( expression ) statement
(6.8.5) iteration-statement:
while ( expression ) statement
do statement while ( expression ) ;
for ( expressionopt ; expressionopt ; expressionopt ) statement
for ( declaration expressionopt ; expressionopt ) statement
(6.8.6) jump-statement:
goto identifier ;
continue ;
break ;
return expressionopt ;


