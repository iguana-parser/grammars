/**
 * 
 * Derived from the C# language specification version 5:
 * http://www.microsoft.com/en-us/download/details.aspx?id=7029
 *
 *
 *  author: Ali Afroozeh
 */
 
module CSharp5


// Basic concepts

syntax NamespaceName
     = NamespaceOrTypeName
     ;

syntax TypeName 
     = NamespaceOrTypeName
     ;

syntax NamespaceOrTypeName 
     = Identifier   TypeArgumentList?
     | NamespaceOrTypeName   "."   Identifier   TypeArgumentList?
     | QualifiedAliasMember
     ;

// Types

syntax Type
     = ValueType
     | ReferenceType 
     | TypeParameter
     | PointerType
     ;
     
syntax PointerType
     = UnmanagedType   "*"
     | "void"   "*"
     ;

syntax UnmanagedType
     = Type
	 ;     

syntax ValueType
     = StructType
     | EnumType
     ;

syntax StructType
     = TypeName
     | SimpleType 
     | NullableType
     ;

syntax SimpleType
     = NumericType
     | "bool"
     ;

syntax NumericType
     = IntegralType
     | FloatingPointType
     | "decimal"
     ;

syntax IntegralType
     = "sbyte"
     | "byte"
     | "short"
     | "ushort"
     | "int"
     | "uint"
     | "long"
     | "ulong"
     | "char"
     ;

syntax FloatingPointType
     = "float"
     | "double"
     ;

syntax NullableType
     = NonNullableValueType  "?"
     ;

syntax NonNullableValueType
     = Type
     ;

syntax EnumType
     = TypeName
     ;

syntax ReferenceType
     = ClassType
     | IinterfaceType
     | ArrayType
     | DelegateType
     ;

syntax ClassType
     = TypeName
     | "object"
     | "dynamic"
     | "string"
     ;

syntax InterfaceType
     = TypeName
     ;
     
syntax RankSpecifier
     = "["   ","*   "]"
     ;

syntax DelegateType
     = TypeName
     ;

syntax TypeArgumentList
     = "\<"   {TypeArgument ","}+   "\>"
     ;

syntax TypeArgument
     = Type
     ;

syntax TypeParameter
     = Identifier
     ;
     

// Variables

syntax VariableReference
     = Expression
     ;


// Expressions

syntax ArgumentList
     = Argument
     | ArgumentList   ","   Argument
     ;

syntax Argument
     = ArgumentName?   ArgumentValue
     ;

syntax ArgumentName
     = Identifier  ":" 
     ;

syntax ArgumentValue
     = Expression
     | "ref"   VariableReference
     | "out"   VariableReference
     ;

syntax PrimaryExpression 
     = PrimaryNoArrayCreationExpression
     | ArrayCreationExpression
     ;

syntax PrimaryNoArrayCreationExpression
     = Literal
     | SimpleName
     | ParenthesizedExpression
     | MemberAccess
     | InvocationExpression
     | ElementAccess
     | ThisAccess
     | BaseAccess
     | PostIncrementExpression
     | PostDecrementExpression
     | ObjectCreationExpression
     | DelegateCreationExpression
     | AnonymousObjectCreationExpression
     | TypeofExpression
     | CheckedExpression
     | UncheckedExpression 
     | DefaultValueExpression
     | AnonymousMethodExpression
     | PointerMemberAccess
     | PointerElementAccess
     | SizeofExpression
     ;

syntax SimpleName
     = Identifier   TypeArgumentList?
     ;

syntax ParenthesizedExpression
     = "("   Expression   ")"
     ;

syntax MemberAccess
     = PrimaryExpression   "."   Identifier  TypeArgumentList?
     | PredefinedType   "."   Identifier  TypeArgumentList?
     | QualifiedAliasMember   "."   Identifier
     ;

syntax PredefinedType
     = "bool"
     | "byte"
     | "char"
     | "decimal"
     | "double"
     | "float"
     | "int"
     | "long"
     | "object"
     | "sbyte"
     | "short"
     | "string"
     | "uint"
     | "ulong"
     | "ushort"
     ; 

syntax InvocationExpression
     = PrimaryExpression   "("   ArgumentList?   ")"
     ;

syntax ElementAccess
     = PrimaryNoArrayCreationExpression   "["   ArgumentList   "]"
     ;

syntax ThisAccess
     = "this"
     ;

syntax BaseAccess
     = "base"   "."   Identifier
     | "base"   "["   ArgumentList   "]"
     ;

syntax PostIncrementExpression
     = PrimaryExpression   "++"
     ;

syntax PostDecrementExpression
     = PrimaryExpression   "--"
     ;

syntax ObjectCreationExpression
     = "new"   Type   "("   ArgumentList?   ")"   ObjectOrCollectionInitializer? 
     | "new"   Type   ObjectOrCollectionInitializer
     ;

syntax ObjectOrCollectionInitializer
     = ObjectInitializer
     | CollectionInitializer
     ;

syntax ObjectInitializer
     = "{"  MemberInitializerList?   "}"
     | "{"   MemberInitializerList   ","   "}"
     ;

syntax MemberInitializerList
     = { MemberInitializer ","}+
     ;

syntax MemberInitializer
     = Identifier   "="   InitializerValue
     ;

syntax InitializerValue
     = Expression
     | ObjectOrCollectionInitializer
     ;

syntax CollectionInitializer
     = "{"   ElementInitializerList   "}"
     | "{"   ElementInitializerList   ","   "}"
     ;

syntax ElementInitializerList
     = { ElementInitializer ","}+
     ;

syntax ElementInitializer
     = NonAssignmentExpression
     | "{"   ExpressionList   "}"
     ;

syntax ExpressionList
     = { Expression ","}+
     ;

syntax ArrayCreationExpression
     = "new"   NonArrayType   "["   ExpressionList   "]"   RankSpecifier*   ArrayInitializer?
     | "new"   ArrayType   ArrayInitializer 
     | "new"   RankSpecifier   ArrayInitializer
     ;

syntax DelegateCreationExpression
     = "new"   DelegateType   "("   Expression   ")"
     ;

syntax AnonymousObjectCreationExpression
     = "new"   AnonymousObjectInitializer
     ;

syntax AnonymousObjectInitializer
     = "{"   MemberDeclaratorList?   "}"
     | "{"   MemberDeclaratorList   ","   "}"
     ;

syntax MemberDeclaratorList
     = { MemberDeclarator "," }+
     ;

syntax MemberDeclarator
     = SimpleName
     | MemberAccess
     | Identifier   "="   Expression
     ;

syntax TypeofExpression
     = "typeof"   "("   type   ")"
     | "typeof"   "("   UnboundTypeName   ")"
     | "typeof" "(" "void" ")"
     ; 

syntax UnboundTypeName
     = Identifier   GenericDimensionSpecifier?
     | Identifier   "::"   Identifier   GenericDimensionSpecifier?
     | UnboundTypeName   "."   Identifier   GenericDimensionSpecifier?
     ;

syntax GenericDimensionSpecifier
     = "\<"   ","*   "\>"
     ;

syntax CheckedExpression
     = "checked"   "("   Expression   ")"
     ;

syntax UncheckedExpression
     = "unchecked"  "("   Expression   ")"
     ;

syntax DefaultValueExpression
     = "default"   "("   Type   ")"
     ;

syntax UnaryExpression
     = PrimaryExpression
     | "+"   UnaryExpression
     | "-"   UnaryExpression
     | "!"   UnaryExpression
     | "~"   UnaryExpression
     | PreIncrementExpression
     | PreDecrementExpression
     | CastExpression
     | PointerIndirectionExpression
     | AddressofExpression
     ;
     
syntax PointerIndirectionExpression
     = "*"   UnaryExpression
     ;

syntax PointerMemberAccess
     = PrimaryExpression   "\>"   Identifier  TypeArgumentListopt
     ;

syntax PointerElementAccess
     = PrimaryNoArrayCreationExpression   "["   Expression   "]"
     ;

syntax AddressofExpression
     = "&"   UnaryExpression
     ;

syntax SizeofExpression
     = "sizeof"   "("   UnmanagedType   ")"
     ;

syntax FixedStatement
     = "fixed"   "("   PointerType   FixedPointerDeclarators   ")"   EmbeddedStatement
     ;

syntax FixedPointerDeclarators
     = { FixedPointerDeclarator ","}+
     ;

syntax FixedPointerDeclarator
     = Identifier   "="   FixedPointerInitializer
     ;

syntax FixedPointerInitializer
     = "&"   VariableReference
     | Expression
     ;
     

syntax PreIncrementExpression
     = "++"   UnaryExpression
     ;

syntax PreDecrementExpression
     = "--"   UnaryExpression
     ;

syntax CastExpression
     = "("   Type   ")"   UnaryExpressin
     ;

syntax MultiplicativeExpression
     = UnaryExpression
     | MultiplicativeExpression   "*"   UnaryExpression
     | MultiplicativeExpression   "/"   UnaryExpression
     | MultiplicativeExpression   "%"   UnaryExpression
     ;

syntax AdditiveExpression
     = MultiplicativeExpression
     | AdditiveExpression   "+"   MultiplicativeExpression
     | AdditiveExpression   "–"   MultiplicativeExpression
     ;
     
syntax ShiftExpression
     = AdditiveExpression 
     | ShiftExpression   "\<\<"   AdditiveExpression
     | ShiftExpression   RightShift   AdditiveExpression
     ;

syntax RelationalExpression
     = ShiftExpression
     | RelationalExpression   "\<"   ShiftExpression
     | RelationalExpression   "\>"   ShiftExpression
     | RelationalExpression   "\<="   ShiftExpression
     | RelationalExpression   "\>="   ShiftExpression
     | RelationalExpression   "is"   Type
     | RelationalExpression   "as"   Type
     ;

syntax EqualityExpression
     = RelationalExpression
     | EqualityExpression   "=="   RelationalExpression
     | EqualityExpression   "!="   RelationalExpression
     ;

syntax AndExpression
     = EqualityExpression
     | AndExpression   "&"   EqualityExpression
     ;

syntax ExclusiveOrExpression
     = AndExpression
     | ExclusiveOrExpression   "^"   AndExpression
     ;

syntax InclusiveOrExpression
     = ExclusiveOrExpression
     | InclusiveOrExpression   "|"   ExclusiveOrExpression
     ;

syntax ConditionalAndExpression
     = InclusiveOrExpression
     | ConditionalAndExpression   "&&"   InclusiveOrExpression
     ;

syntax ConditionalOrExpression
     = ConditionalAndExpression
     | ConditionalOrExpression   "||"   ConditionalAndExpression
     ;

syntax NullCoalescingExpression
     = ConditionalOrExpression
     | ConditionalOrExpression   "??"   NullCoalescingExpression
     ;

syntax ConditionalExpression
     = NullCoalescingExpression
     | NullCoalescingExpression   "?"   Expression   ":"   Expression
     ;

syntax LambdaExpression
     = AnonymousFunctionSignature   "=\>"   AnonymousFunctionBody
     ;

syntax AnonymousMethodExpression
     = "delegate"   ExplicitAnonymousFunctionSignatureopt   Block
     ;

syntax AnonymousFunctionSignature
     = ExplicitAnonymousFunctionSignature 
     | ImplicitAnonymousFunctionSignature
     ;
     
     
syntax ExplicitAnonymousFunctionSignature
     = "("   ExplicitAnonymousFunctionParameterList?   ")"
     ;

syntax ExplicitAnonymousFunctionParameterList
     = { ExplicitAnonymousFunctionParameter "," }+
     ;

syntax ExplicitAnonymousFunctionParameter
     = AnonymousFunctionParameterModifier?   Type   Identifier
     ;

syntax AnonymousFunctionParameterModifier 
     = "ref"
     | "out"
     ;

syntax ImplicitAnonymousFunctionSignature
     = "("   ImplicitAnonymousFunctionParameterList?   ")"
     | ImplicitAnonymousFunctionParameter
     ;

syntax ImplicitAnonymousFunctionParameterList
     = { ImplicitAnonymousFunctionParameter ","}+
     ;

syntax ImplicitAnonymousFunctionParameter
     = Identifier
     ;

syntax AnonymousFunctionBody
     = Expression
     | Block
     ;

syntax QueryExpression
     = FromClause   QueryBody
     ;

syntax FromClause
     = "from"   Type?   Identifier   "in"   Expression
     ;

syntax QueryBody
     = QueryBodyClause*   SelectOrGroupClause   QueryContinuation?
     ;

syntax QueryBodyClause
     = FromClause
     | LetClause
     | WhereClause
     | JoinClause
     | JoinIntoClause
     | OrderbyClause
     ;

syntax LetClause
     = "let"   Identifier   "="   Expression
     ; 

syntax WhereClause
     = "where"   BooleanExpression
     ;

syntax JoinClause
     = "join"   Type?   Identifier   "in"   Expression   "on"   Expression   "equals"   Expression 
     ;
     

syntax JoinIntoClause
     = "join"   Type?   Identifier   "in"   Expression   "on"   Expression   "equals"   Expression   "into"   Identifier
     ;

syntax OrderbyClause
     = "orderby"   Orderings
     ;

syntax Orderings
     = { Ordering ","}+
     ;

syntax Ordering
     = Expression    OrderingDirection?
     ;

syntax OrderingDirection
     = "ascending"
     | "descending"
     ;

syntax SelectOrGroupClause
     = SelectClause
     | GroupClause
     ;

syntax SelectClause
     = "select"   Expression
     ;

syntax GroupClause
     = "group"   Expression   "by"   Expression
     ;

syntax QueryContinuation
     = "into"   Identifier   QueryBody
     ;

syntax Assignment
     = UnaryExpression   AssignmentOperator   Expression
     ;

syntax AssignmentOperator
     = "="
     | "+="
     | "="
     | "*="
     | "/="
     | "%="
     | "&="
     | "|="
     | "^="
     | "\<\<="
     | RightShiftAssignment
     ;

syntax Expression 
     = NonAssignmentExpression
     | Assignment
     ;

syntax NonAssignmentExpression
     = ConditionalExpression
     | LambdaExpression
     | QueryExpression
     ;

syntax ConstantExpression
     = Expression
     ;


syntax BooleanExpression
     = Expression
     ;
     
// Statements

syntax Statement
     = LabeledStatement
     | DeclarationStatement
     | EmbeddedStatement
     ;

syntax EmbeddedStatement
     = Block
     | EmptyStatement
     | ExpressionStatement
     | SelectionStatement
     | IterationStatement
     | JumpStatement
     | TryStatement
     | CheckedStatement
     | UncheckedStatement
     | LockStatement
     | UsingStatement 
     | YieldStatement
     | UnsafeStatement 
     | FixedStatement
     ;

syntax UnsafeStatement
     = "unsafe"   Block
     ;

syntax Block
     = "{"   StatementList?   "}"
     ;

syntax StatementList
     = Statement
     | StatementList   Statement
     ;

syntax EmptyStatement
     = ";"
     ;

syntax LabeledStatement
     = Identifier  ":"    Statement
     ;

syntax DeclarationStatement
     = LocalVariableDeclaration   ";"
     | LocalConstantDeclaration   ";"
     ;

syntax LocalVariableDeclaration
     = LocalVariableType   LocalVariableDeclarators
     ;

syntax LocalVariableType
     = Type
     | "var"
     ;

syntax LocalVariableDeclarators
     = { LocalVariableDeclarator "," }+
     ;

syntax LocalVariableDeclarator
     = { Identifier "=" }+
     ;
      
syntax LocalVariableInitializer
     = Expression
     | ArrayInitializer
     ;

syntax LocalConstantDeclaration
     = "const"   Type   ConstantDeclarators
     ;

syntax ConstantDeclarators
     = ConstantDeclarator
     | ConstantDeclarators   ","   ConstantDeclarator
     ;

syntax ConstantDeclarator
     = Identifier   "="   ConstantExpression
     ;

syntax ExpressionStatement
     = StatementExpression   ";"
     ;

syntax StatementExpression
     = InvocationExpression
     | ObjectCreationExpression
     | Assignment
     | PostIncrementExpression
     | PostDecrementExpression
     | PreIncrementExpression
     | PreDecrementExpression
     ;

syntax SelectionStatement
     = IfStatement
     | SwitchStatement
     ;

syntax IfStatement
     = "if"   "("   BooleanExpression   ")"   EmbeddedStatement
     | "if"   "("   BooleanExpression   ")"   EmbeddedStatement   "else"   EmbeddedStatement
     ;

syntax SwitchStatement
     = "switch"   "("   Expression   ")"   SwitchBlock
     ;

syntax SwitchBlock
     = "{"   SwitchSection*   "}"
     ;


syntax SwitchSection
     = SwitchLabel+   StatementList
     ;

syntax SwitchLabel
     = "case"   ConstantExpression ":"  
     | "default"  ":"
     ;
     
syntax IterationStatement
     = WhileStatement
     | DoStatement
     | ForStatement
     | ForeachStatement
     ;

syntax WhileStatement
     = "while"   "("   BooleanExpression   ")"   EmbeddedStatement
     ;

syntax DoStatement
     = "do"   EmbeddedStatement   "while"   "("   BooleanExpression   ")"   ";"
     ;

syntax ForStatement
     = "for"   "("   ForInitializer?   ";"   ForCondition?   ";"   ForIterator?   ")"   EmbeddedStatement
     ;

syntax ForInitializer
     = LocalVariableDeclaration
     | StatementExpressionList
     ;

syntax ForCondition
     = BooleanExpression
     ;

syntax ForIterator
     = StatementExpressionList
     ;

syntax StatementExpressionList
     = { StatementExpression "," }+
     ;

syntax ForeachStatement
     = "foreach"   "("   LocalVariableType   Identifier   "in"   Expression   ")"   EmbeddedStatement
     ;

syntax JumpStatement
     = BreakStatement
     | ContinueStatement
     | GotoStatement
     | ReturnStatement
     | ThrowStatement
     ;

syntax BreakStatement
     = "break"   ";"
     ;

syntax ContinueStatement
     = "continue"   ";"
     ;

syntax GotoStatement
     = "goto"   Identifier   ";"
     | "goto"   "case"   ConstantExpression   ";"
     | "goto"   "default"   ";"
     ;

syntax ReturnStatement
     = "return"   Expression?   ";"
     ;

syntax ThrowStatement
     = "throw"   Expression?   ";"
     ;
     
syntax TryStatement
     = "try"   "block"   CatchClauses
     | "try"   "block"   FinallyClause
     | "try"   "block"   CatchClauses   FinallyClause
     ;

syntax CatchClauses
     = SpecificCatchClause+   GeneralCatchClause?
     | SpecificCatchClause*   GeneralCatchClause
     ;

syntax SpecificCatchClause
     = "catch"   "("   ClassType   Identifier?   ")"   "block"
     ;

syntax GeneralCatchClause
     = "catch"   "block"
     ;

syntax FinallyClause
     = "finally"   "block"
     ;

syntax CheckedStatement
     = "checked"   "block"
     ;

syntax UncheckedStatement
     = "unchecked"   "block"
     ;

syntax LockStatement
     = "lock"   "("   Expression   ")"   EmbeddedStatement
     ;

syntax UsingStatement
     = "using"   "("    ResourceAcquisition   ")"    EmbeddedStatement
     ;

syntax ResourceAcquisition
     = LocalVariableDeclaration
     | Expression
     ;

syntax YieldStatement
     = "yield"   "return"   Expression   ";"
     | "yield"   "break"   ";"
     ;
     
     
// Namespaces

syntax CompilationUnit
     = ExternAliasDirectives?   UsingDirectives?  GlobalAttributes? NamespaceMemberDeclaration*
	; 

syntax NamespaceDeclaration
     = "namespace"   QualifiedIdentifier   NamespaceBody   ";"?
     ;

syntax QualifiedIdentifier
     = { Identifier "." }+
     ;

syntax NamespaceBody
     = "{"   ExternAliasDirective*   UsingDirectives?   NamespaceMemberDeclaration*   "}"
     ;

syntax ExternAliasDirective
     = "extern"   "alias"   Identifier   ";"
     ;

syntax UsingDirectives
     = UsingDirective
     | UsingDirectives   UsingDirective
     ;

syntax UsingDirective
     = UsingAliasDirective
     | UsingNamespaceDirective
     ;

syntax UsingAliasDirective
     = "using"   Identifier   "="   NamespaceOrTypeName   ";"
     ;

syntax UsingNamespaceDirective
     = "using"   NamespaceName   ";"
     ;


syntax NamespaceMemberDeclaration
     = NamespaceDeclaration
     | TypeDeclaration
     ;

syntax TypeDeclaration
     = ClassDeclaration
     | StructDeclaration
     | InterfaceDeclaration
     | EnumDeclaration
     | DelegateDeclaration
     ;

syntax QualifiedAliasMember
     = Identifier  "::"    Identifier   TypeArgumentList?
     ;
     
     
// Classes
syntax ClassDeclaration
     = Attributes?   ClassModifier*   "partial"?   "class"   Identifier   TypeParameterList?
          ClassBase?   TypeParameterConstraintsClause*   ClassBody   ";"?
     ;

syntax ClassModifier
     = "new"
     | "public"
     | "protected"
     | "internal"
     | "private"
     | "abstract"
     | "sealed"
     | "static"
     | "unsafe"
     ;

syntax TypeParameterList
     = "\<"   TypeParameters   "\>"
     ;

syntax TypeParameters
     = Attributes?   TypeParameter
     | TypeParameters   ","   Attributes?   TypeParameter
     ;

syntax TypeParameter
     = Identifier
     ;

syntax ClassBase
     = ":" ClassType
     | ":" InterfaceTypeList
     | ":" ClassType   ","   InterfaceTypeList
     ;

syntax InterfaceTypeList
     = { InterfaceType "," }+
     ;

syntax TypeParameterConstraintsClause
     = "where"   TypeParameter   ":"   TypeParameterConstraints
     ;

syntax TypeParameterConstraints
     = PrimaryConstraint
     | SecondaryConstraints
     | ConstructorConstraint
     | PrimaryConstraint   ","   SecondaryConstraints
     | PrimaryConstraint   ","   ConstructorConstraint
     | SecondaryConstraints   ","   ConstructorConstraint
     | PrimaryConstraint   ","   SecondaryConstraints   ","   ConstructorConstraint
     ;

syntax PrimaryConstraint
     = ClassType
     | "class"
     | "struct"
     ;

syntax SecondaryConstraints
     = InterfaceType
     | TypeParameter
     | SecondaryConstraints   ","   InterfaceType
     | SecondaryConstraints   ","   TypeParameter
     ;

syntax ConstructorConstraint
     = "new"   "("   ")"
     ;

syntax ClassBody
     = "{"   ClassMemberDeclaration*   "}"
     ;

 syntax ClassMemberDeclaration
     = ConstantDeclaration
     | FieldDeclaration
     | MethodDeclaration
     | PropertyDeclaration
     | EventDeclaration
     | IndexerDeclaration
     | OperatorDeclaration
     | ConstructorDeclaration
     | DestructorDeclaration
     | StaticConstructorDeclaration
     | TypeDeclaration
     ;

syntax ConstantDeclaration
     = Attributes?   ConstantModifier*   "const"   Type   ConstantDeclarators   ";"
     ;

syntax ConstantModifier
     = "new"
     | "public"
     | "protected"
     | "internal"
     | "private"
     ;

syntax ConstantDeclarators
     = { ConstantDeclarator "," }+
     ;

syntax ConstantDeclarator
     = Identifier   "="   ConstantExpression
     ;

syntax FieldDeclaration
     = Attributes?   FieldModifier*   Type   VariableDeclarators   ";"
     ;

syntax FieldModifier
     = "new"
     | "public"
     | "protected"
     | "internal"
     | "private"
     | "static"
     | "readonly"
     | "volatile"
     | "unsafe"
     ;

syntax VariableDeclarators
     = { VariableDeclarator ","}+
     ;

 syntax VariableDeclarator
     = Identifier
     | Identifier   "="   VariableInitializer
     ;

syntax VariableInitializer
     = Expression
     | ArrayInitializer
     ;

syntax MethodDeclaration
     = MethodHeader   MethodBody
     ;

syntax MethodHeader
     = Attributes?   MethodModifier*   "partial"?   ReturnType   MemberName   TypeParameterList?
		"("   FormalParameterList?   ")"   TypeParameterConstraintsClauses?
	  ;

syntax MethodModifier
     = "new"
     | "public"
     | "protected"
     | "internal"
     | "private"
     | "static"
     | "virtual"
     | "sealed"
     | "override"
     | "abstract"
     | "extern"
     | "unsafe"
     ;

syntax ReturnType
     = Type
     | "void"
     ;

syntax MemberName
     = Identifier
     | InterfaceType   "."   Identifier
     ;

syntax MethodBody
     = Block
     | ";"
     ;

syntax FormalParameterList
     = FixedParameters
     | FixedParameters   ","   ParameterArray
     | ParameterArray
     ;

syntax FixedParameters
     = { FixedParameter ","}+
     ;

syntax FixedParameter
     = Attributes?   ParameterModifier?   Type   Identifier   DefaultArgument?
     ;

syntax DefaultArgument
     =  Expression
     ;

syntax ParameterModifier
     = "ref"
     | "out"
     | "this"
     ;

syntax ParameterArray
     = Attributes?   "params"   ArrayType   Identifier
     ;

syntax PropertyDeclaration
     = Attributes?   PropertyModifier*   Type   MemberName   "{"   AccessorDeclarations   "}"
     ;

syntax PropertyModifier
     = "new"
     | "public"
     | "protected"
     | "internal"
     | "private"
     | "static"
     | "virtual"
     | "sealed"
     | "override"
     | "abstract"
     | "extern"
     | "unsafe"
     ;

syntax MemberName
     = Identifier
     | InterfaceType   "."   Identifier
     ;

syntax AccessorDeclarations
     = GetAccessorDeclaration   SetAccessorDeclaration?
     | SetAccessorDeclaration   GetAccessorDeclaration?
     ;

syntax GetAccessorDeclaration
     = Attributes?   AccessorModifier?    "get"   AccessorBody
     ;

syntax SetAccessorDeclaration
     = Attributes?   AccessorModifier?   "set"   AccessorBody
     ;

syntax AccessorModifier
     = "protected"
     | "internal"
     | "private"
     | "protected"   "internal"
     | "internal"   "protected"
     ;

syntax AccessorBody
     = Block
     | ";"
     ;

syntax EventDeclaration
     = Attributes?   EventModifier*   "event"   Type   VariableDeclarators   ";"
     | Attributes?   EventModifier*   "event"   Type   MemberName   "{"   EventAccessorDeclarations   "}"
     ;
     

syntax EventModifier
     = "new"
     | "public"
     | "protected"
     | "internal"
     | "private"
     | "static"
     | "virtual"
     | "sealed"
     | "override"
     | "abstract"
     | "extern"
     | "unsafe"
     ;

syntax EventAccessorDeclarations
     = AddAccessorDeclaration   RemoveAccessorDeclaration
     | RemoveAccessorDeclaration   AddAccessorDeclaration
     ;

syntax AddAccessorDeclaration
     = Attributes?   "add"   Block
     ;

syntax RemoveAccessorDeclaration
     = Attributes?   "remove"   Block
     ;

syntax IndexerDeclaration
     = Attributes?   IndexerModifier*   IndexerDeclarator   "{"   AccessorDeclarations   "}"
     ;

syntax IndexerModifier
     = "new"
     | "public"
     | "protected"
     | "internal"
     | "private "
     | "virtual"
     | "sealed"
     | "override"
     | "abstract"
     | "extern"
     | "unsafe"
     ;

syntax IndexerDeclarator
     = Type   "this"   "["   FormalParameterList   "]"
     | Type   InterfaceType   "."   "this"   "["   FormalParameterList   "]"
     ;

syntax OperatorDeclaration
     = Attributes?   OperatorModifier+   OperatorDeclarator   OperatorBody
     ;


syntax OperatorModifier
     = "public"
     | "static"
     | "extern"
     | "unsafe"
     ;

syntax OperatorDeclarator
     = UnaryOperatorDeclarator
     | BinaryOperatorDeclarator
     | ConversionOperatorDeclarator
     ;

syntax UnaryOperatorDeclarator
     = Type   "operator"   OverloadableUnaryOperator   "("   Type   Identifier   ")"
     ;

syntax OverloadableUnaryOperator
     = "+"
     | "!"
     | "~"   
     | "++" 
     | "-"   
     | "true"
     | "false"
     ;

syntax BinaryOperatorDeclarator
     = Type   "operator"   OverloadableBinaryOperator   "("   Type   Identifier   ","   Type   Identifier   ")"
     ;

syntax OverloadableBinaryOperator
     = "+"
     | "-"
     | "*"
     | "/"
     | "%"
     | "&"
     | "|"
     | "^"
     | "\<\<"
     | RightShift
     | "=="
     | "!="
     | "\>"
     | "\<"
     | "\>="
     | "\<="
     ;

syntax ConversionOperatorDeclarator
     = "implicit"   "operator"   Type   "("   Type   Identifier   ")"
     | "explicit"   "operator"   Type   "("   Type   Identifier   ")"
     ;

syntax OperatorBody
     = Block
     | ";"
     ;

syntax ConstructorDeclaration
     = Attributes?   ConstructorModifier*   ConstructorDeclarator   ConstructorBody
     ;


syntax ConstructorModifier
     = "public"
     | "protected"
     | "internal"
     | "private"
     | "extern"
     | "unsafe"
     ;

syntax ConstructorDeclarator
     = Identifier   "("   FormalParameterList?   ")"   ConstructorInitializer?
     ;

syntax ConstructorInitializer
        = ":" "base"   "("   ArgumentList?   ")"
        | ":" "this"   "("   ArgumentList?   ")"
        ;

syntax ConstructorBody
     = Block
     | ";"
     ;

syntax StaticConstructorDeclaration
     = Attributes?   StaticConstructorModifiers  Identifier   "("   ")"   StaticConstructorBody
     ;

syntax StaticConstructorModifiers
     = "extern"?   "static"
     | "static"    "extern"?
     | "extern"?   "unsafe"?   "static"
     | "unsafe"?   "extern"?   "static"
     | "extern"?   "static"    "unsafe"?
     | "unsafe"?   "static"    "extern"?
     | "static"    "extern"?   "unsafe"?
     | "static"    "unsafe"?   "extern"?
     ;

syntax StaticConstructorBody
     = Block
     | ";"
     ;

syntax DestructorDeclaration
     = Attributes?   "extern"?   "~"   Identifier   "("   ")"    DestructorBody
     | Attributes?   "extern"?   "unsafe"?   "~"   "identifier"   "("   ")"    DestructorBody
     | Attributes?   "unsafe"?   "extern"?   "~"   "identifier"   "("   ")"    DestructorBody
     ;

syntax DestructorBody
     = Block
     | ";"
     ;
     

// Structs     

syntax StructDeclaration
     = Attributes?   StructModifier*   "partial"?   "struct"   Identifier   TypeParameterList?
        StructInterfaces?   TypeParameterConstraintsClauses?   StructBody   ";"?
     ;

syntax StructModifier
     = "new"
     | "public"
     | "protected"
     | "internal"
     | "private"
     | "unsafe"
     ; 

syntax StructInterfaces
     = ":" InterfaceTypeList
     ;

syntax StructBody
     = "{"   StructMemberDeclarations?   "}"
     ;

syntax StructMemberDeclarations
     = StructMemberDeclaration
     | StructMemberDeclarations   StructMemberDeclaration
     | FixedSizeBufferDeclaration
     ;

syntax StructMemberDeclaration
     = ConstantDeclaration
     | FieldDeclaration
     | MethodDeclaration
     | PropertyDeclaration
     | EventDeclaration
     | IndexerDeclaration
     | OperatorDeclaration
     | ConstructorDeclaration
     | StaticConstructorDeclaration
     | TypeDeclaration
     ;
     
syntax FixedSizeBufferDeclaration
     = Attributes?   FixedSizeBufferModifier*   "fixed"   BufferElementType
        FixedSizeBufferDeclarator+   ";"
     ;

syntax FixedSizeBufferModifier
     = "new"
     | "public"
     | "protected"
     | "internal"
     | "private"
     | "unsafe"
     ;

syntax BufferElementType
     = Type
     ;

syntax FixedSizeBufferDeclarator
     = Identifier   "["   ConstantExpression   "]"
     ;     

// Arrays
syntax ArrayType
     = NonArrayType   RankSpecifier+
     ;

syntax NonArrayType
     = Type
     ;

syntax RankSpecifier
     = "["   ","*   "]"
     ;


syntax ArrayInitializer
     = "{"   VariableInitializerList?   "}"
     | "{"   VariableInitializerList   ","   "}"
     ;

syntax VariableInitializerList
     = { VariableInitializer ","}+
     ;

syntax VariableInitializer
     = Expression
     | ArrayInitializer
     ;

// Interfaces

syntax InterfaceDeclaration
     = Attributes?   InterfaceModifiers*   "partial"?   "interface"   
        Identifier   VariantTypeParameterList?   InterfaceBase?
        TypeParameterConstraintsClause*   InterfaceBody   ";"?
     ;

syntax InterfaceModifier
     = "new"
     | "public"
     | "protected"
     | "internal"
     | "private"
     | "unsafe"
     ;

syntax VariantTypeParameterList
     = "\<"   VariantTypeParameters   "\>"
     ;

syntax VariantTypeParameters
     = Attributes?  VarianceAnnotation?  TypeParameter
     | VariantTypeParameters   ","   Attributes?   VarianceAnnotation?  TypeParameter
     ;

syntax VarianceAnnotation
     = "in"
     | "out"
     ;

syntax InterfaceBase
     = ":"   interfaceTypeList
     ;

syntax InterfaceBody
     = "{"   InterfaceMemberDeclaration*   "}"
     ;

syntax InterfaceMemberDeclaration
     = InterfaceMethodDeclaration
     | InterfacePropertyDeclaration
     | InterfaceEventDeclaration
     | InterfaceIndexerDeclaration
     ;

syntax InterfaceMethodDeclaration
     = Attributes?   "new"?   ReturnType   Identifier   TypeParameterList
        "("   FormalParameterList?   ")"   TypeParameterConstraintsClause*   ";"
     ;

syntax InterfacePropertyDeclaration
     = Attributes?   "new"?   Type   Identifier   "{"   InterfaceAccessors   "}"
     ;

syntax InterfaceAccessors
     = Attributes?   "get"   ";"
     | Attributes?   "set"   ";"
     | Attributes?   "get"   ";"   Attributes?   "set"   ";"
     | Attributes?   "set"   ";"   Attributes?   "get"   ";"
     ;

syntax InterfaceEventDeclaration
     = Attributes?   "new"?   "event"   Type   Identifier   ";"
     ;

syntax InterfaceIndexerDeclaration
     = Attributes?   "new"?   Type   "this"   "["   FormalParameterList   "]"   "{"   InterfaceAccessors   "}"
     ; 

// Enums

syntax EnumDeclaration
     = Attributes?   EnumModifier*   "enum"   Identifier   EnumBase?   EnumBody   ";"?
     ;

syntax EnumBase
     = ":"   IntegralType
     ;

syntax EnumBody
     = "{"   EnumMemberDeclarations?   "}"
     | "{"   EnumMemberDeclarations   ","   "}"
     ;

syntax EnumModifier
     = "new"
     | "public"
     | "protected"
     | "internal"
     | "private"
     ;

syntax EnumMemberDeclarations
     = { EnumMemberDeclaration "," }+
     ;

syntax EnumMemberDeclaration
     = Attributes?   Identifier
     | Attributes?   Identifier   "="   ConstantExpression
     ;


// Delegates

syntax DelegateDeclaration
     = Attributes?   DelegateModifier*   "delegate"   ReturnType   
        Identifier  VariantTypeParameterList?   
        "("   FormalParameterList?   ")"   TypeParameterConstraintsClause*   ";"
     ;

syntax DelegateModifier
     = "new"
     | "public"
     | "protected"
     | "internal"
     | "private"
     | "unsafe"
     ;
     
// Attributes

syntax GlobalAttributes
    = GlobalAttributeSection+
    ;

syntax GlobalAttributeSection
    = "["   GlobalAttributeTargetSpecifier   AttributeList   "]"
    | "["   GlobalAttributeTargetSpecifier   AttributeList   ","   "]"
    ;


syntax GlobalAttributeTargetSpecifier
    = GlobalAttributeTarget  ":"
    ;

syntax GlobalAttributeTarget
    = "assembly"
    | "module"
    ;

syntax Attributes
    = AttributeSection+
    ;

syntax AttributeSection
    = "["   AttributeTargetSpecifier?   AttributeList   "]"
    | "["   AttributeTargetSpecifier?   AttributeList   ","   "]"
    ;

syntax AttributeTargetSpecifier
    = AttributeTarget   ":"
    ;

syntax AttributeTarget
    = "field"
    | "event"
    | "method"
    | "param"
    | "property"
    | "return"
    | "type"
    ;

syntax AttributeList
    = { Attribute ","}+
    ;

syntax Attribute
    = AttributeName   AttributeArguments?
    ;

syntax AttributeName
    = TypeName
    ;

syntax AttributeArguments
    = "("   PositionalArgumentList?   ")"
    | "("   PositionalArgumentList   ","   NamedArgumentList   ")"
    | "("   NamedArgumentList   ")"
    ;

syntax PositionalArgumentList
    = { PositionalArgument "," }+
    ;

syntax PositionalArgument
    = ArgumentName?   AttributeArgumentExpression
    ;

syntax NamedArgumentList
    = { NamedArgument ","}+
    ;

syntax NamedArgument
    = Identifier   "="   AttributeArgumentExpression
    ;

syntax AttributeArgumentExpression
    = Expression
    ;


//----------------------------------------------------------------------------------------------------------------
// Lexical Definititions
//----------------------------------------------------------------------------------------------------------------

layout Layout 
     = (WhiteSpace | Comment)* !>> [\t \n \r \f  \ ] !>> "/*" !>> "//";

/* 
 * Carriage return character (U+000D)
 * Line feed character (U+000A)
 * Carriage return character (U+000D) followed by line feed character (U+000A)
 * Next line character (U+0085)
 * Line separator character (U+2028)
 * Paragraph separator character (U+2029)
 */
lexical NewLine
      = [\r \n \u0085 \u2028 \u2029]
      ;
      
// Comments

lexical Comment
      = SingleLineComment
      | DelimitedComment
      ;
      
lexical SingleLineComment
      = "//" InputCharacter*
      ;      
      
lexical InputCharacter 
	  = ![] \ [\r \n \u0085 \u2028 \u2029]    // Any Unicode character Except NewLine 
	  | [\a00]                                // to match zero        
      ;
      
lexical DelimitedComment
     = "/*"   DelimitedCommentSection*   [*]+   "/"
     ;

lexical DelimitedCommentSection
     = "/"
     | [*]*  NotSlashOrAsterisk
     ;

lexical NotSlashOrAsterisk
      = ![] \ [/ *]
      ;
      
/* 
 * Any character with Unicode class Zs
 * Horizontal tab character (U+0009)
 * Vertical tab character (U+000B)
 * Form feed character (U+000C)
 */
 lexical Whitespace
       = [\u0020 \u00A0 \u1680 \u2000-\u200A \u202F \u205F \u3000 \u0009 \u000B \u000C]
       ;
       
// B.1.5 Unicode character escape sequences       
 
lexical UnicodeEscapeSequence
      = "\\u"   HexDigit   HexDigit   HexDigit   HexDigit
      | "\\U"   HexDigit   HexDigit   HexDigit  HexDigit   HexDigit   HexDigit   HexDigit   HexDigit
      ;
      
// Identifiers      
      
lexical Identifier
      = IdentifierOrKeyword \ Keyword
      | "@"  IdentifierOrKeyword
      ;
      
      
lexical IdentifierOrKeyword
      = IdentifierStartCharacter IdentifierPartCharacter*
      ;

lexical IdentifierPartCharacter
     = LetterCharacter
     |  "_"
     ;
     
lexical IdentifierPartCharacter
      = LetterCharacter
      | DecimalDigitCharacter
      | ConnectingCharacter
      | CombiningCharacter
      | FormattingCharacter
      ;
     
lexical LetterCharacter
     = Lu | Ll | Lt | Lm | Lo | Nl
     ; 
     
lexical CombiningCharacter
      = Mn | Mc
      ;
      
lexical DecimalDigitCharacter
      = Nd
      ;
       
lexical ConnectingCharacter  
      = Pc
      ;      
      
lexical FormattingCharacter  
      = Cf
      ;
      
      
// Keywords      
  
lexical Keyword  
      = "abstract"   
      | "as"
      | "base"
      | "bool"
      | "break"
      | "byte"
      | "case"
      | "catch"
      | "char"
      | "checked"
      | "class"
      | "const"
      | "continue"
      | "decimal"
      | "default"
      | "delegate"
      | "do"
      | "double"
      | "else"
      | "enum"
      | "event"
      | "explicit"
      | "extern"
      | "false"
      | "finally"
      | "fixed"
      | "float"
      | "for"
      | "foreach"
      | "goto"
      | "if"
      | "implicit"
      | "in"
      | "int"
      | "interface"
      | "internal"
      | "is"
      | "lock"
      | "long"
      | "namespace"
      | "new"
      | "null"
      | "object"
      | "operator"
      | "out"
      | "override"
      | "params"
      | "private"
      | "protected"
      | "public"
      | "readonly"
      | "ref"
      | "return"
      | "sbyte"
      | "sealed"
      | "short"
      | "sizeof"
      | "stackalloc"
      | "static"
      | "string"
      | "struct"
      | "switch"
      | "this"
      | "throw"
      | "true"
      | "try"
      | "typeof"
      | "uint"
      | "ulong"
      | "unchecked"
      | "unsafe"
      | "ushort"
      | "using"
      | "virtual"
      | "void"
      | "volatile"
      | "while"
      ;

// Literals      
      
lexical Literal
     = BooleanLiteral
     | IntegerLiteral
     | RealLiteral
     | CharacterLiteral
     | StringLiteral
     | NullLiteral
     ;
     
lexical BooleanLiteral
      = "true"
      | "false"
      ;
     
lexical IntegerLiteral
     = DecimalIntegerLiteral
     | HexadecimalIntegerLiteral
     ;
     
lexical DecimalIntegerLiteral
     = DecimalDigit+  IntegerTypeSuffix?
     ;
     
lexical DecimalDigit
      = [0-9]
      ;     
     
lexicalIntegerTypeSuffix
      = "U" | "u" | "L" | "l" | "UL" | "Ul" | "uL" | "ul" | "LU" | "Lu" | "lU" | "lu"
      ;
      
lexical HexadecimalIntegerLiteral 
     = [0][xX]   HexDigit+   IntegerTypeSuffix?
     ;      
      
lexical HexDigit
      = [0-9  A-F  a-f]
      ;
      
lexical RealLiteral
     = DecimalDigit+  "."   DecimalDigit+   ExponentPart?   RealTypeSuffix?
     | "."  DecimalDigit+   ExponentPart?   RealTypeSuffix?
     | DecimalDigit+   ExponentPart   RealTypeSuffix?
     | DecimalDigit+   RealTypeSuffix
     ;
            
lexical  ExponentPart
     = [eE]   Sign?   DecimalDigit+
     ;
      
lexical Sign = [+  \-];

lexical RealTypeSuffix = [F  f  D  d  M  m];
      
lexical CharacterLiteral
     = [\']   Character   [\']
     ;
     
lexical Character
      = SingleCharacter
      | SimpleEscapeSequence
      | HexadecimalEscapeSequence
      | UnicodeEscapeSequence
      ; 

lexical SingleCharacter
      = ![] \ [\' \\ \r \n \u0085 \u2028 \u2029]
      ;
      
lexical SimpleEscapeSequence
      = [\\][\']
      | [\\][\"]
      | [\\][\\]
      | [\\][0]
      | [\\][a]
      | [\\][b]
      | [\\][f]
      | [\\][n]
      | [\\][r]
      | [\\][t]
      | [\\][v]
      ;
      
lexical HexadecimalEscapeSequence
     = "\\x"   HexDigit   HexDigit?   HexDigit?   HexDigit?
     ;
     
lexical StringLiteral
      = RegularStringLiteral
      | VerbatimStringLiteral
      ;
      
lexical RegularStringLiteral
      = [\"]   RegularStringLiteralCharacter*   [\"]
      ;
      
lexical RegularStringLiteralCharacter
      = SingleRegularStringLiteralCharacter
      | SimpleEscapeSequence
      | HexadecimalEscapeSequence
      | UnicodeEscapeSequence
      ;
     
lexical SingleRegularStringLiteralCharacter
      = ![] \ [\" \\  \r \n \u0085 \u2028 \u2029]
      ;

lexical VerbatimStringLiteral 
      = "@" [\"]   VerbatimStringLiteralCharacter*   [\"]
      ;

lexical VerbatimStringLiteralCharacter
      = SingleVerbatimStringLiteralCharacter
      | QuoteEscapeSequence
      ;

lexical SingleVerbatimStringLiteralCharacter
     = ![] \ [\"]
     ;

lexical QuoteEscapeSequence
      = [\"][\"]
      ;

lexical NullLiteral
      = "null"
      ;
      
      
// Operators and punctuators   
   
lexical OperatorOrPunctuator
     = "{"
     | "}"
     | "["
     | "]"
     | "("
     | ")"
     | "."
     | ","
     | ":"
     | ";"
     | "+"
     | "-"
     | "*"
     | "/"
     | "%"
     | "&"
     | "|"
     | "^"
     | "!"
     | "~"
     | "="
     | "\<"
     | "\>"
     | "?"
     | "??"
     | "::"
     | "++"
     | "--"
     | "&&"
     | "||"
     | "-\>"
     | "=="
     | "!="
     | "\<="
     | "\>="
     | "+="
     | "-="
     | "*="
     | "/="
     | "%=:"
     | "&="
     | "|="
     | "^="
     | "\<\<"
     | "\<\<="
     | "=\>"
     ;
      
lexical RightShift
      = "\>|\>"
      ;
      
lexical RightShiftAssignment
     = "\>|\>="
     ;
     
// Pre-processing directives

lexical PpDirective
      = PpDeclaration
      | PpConditional
      | PpLine
      | PpDiagnostic
      | PpRegion 
      | PpPragma
      ;
      
lexical ConditionalSymbol
      = IdentifierOrKeyword \ "true" \ "false"
      ;
      
lexical PpExpression
      = Whitespace?   PpOrExpression   Whitespace?
      ;

lexical PpOrExpression
      = PpAndExpression
      | PpOrExpression   Whitespace?   "||"   Whitespace?   PpAndExpression
      ;

lexical PpAndExpression
      = PpEqualityExpression
      | PpAndExpression   Whitespace?   "&&"   Whitespace?   PpEqualityExpression
      ;

lexical PpEqualityExpression
      = PpUnaryExpression
      | PpEqualityExpression   Whitespace?   "=="   Whitespace?   PpUnaryExpression
      | PpEqualityExpression   Whitespace?   "!="   Whitespace?   PpUnaryExpression
      ;

lexical PpUnaryExpression
      = PpPrimaryExpression
      | "!"   Whitespace?   PpUnaryExpression
      ;
      
lexical PpPrimaryExpression
     = "true"
     | "false"
     ;
     
lexical ConditionalSymbol
     = "("   Whitespace?   PpExpression   Whitespace?   ")"
     ;

lexical PpDeclaration
      = Whitespace?   "#"   Whitespace?   ("define" | "undef")   Whitespace   ConditionalSymbol   PpNewLine
      ;
      
lexical PpNewLine 
      = Whitespace?   SingleLineComment?   NewLine
      ;

lexical PpConditional 
      = PpIfSection   PpElifSection*   PpElseSection?   PpEndif
      ;

lexical PpIfSection 
      = Whitespace?   "#"   Whitespace?   "if"   Whitespace   PpExpression   PpNewLine   ConditionalSection?
      ;

lexical PpElifSection
      = Whitespace?   "#"   Whitespace?   "elif"   Whitespace   PpExpression   PpNewLine   ConditionalSection?
      ;

lexical PpElseSection
      = Whitespace?   "#"   Whitespace?   "else"   PpNewLine   ConditionalSection?
      ;

lexical PpEndif
      = Whitespace?   "#"   Whitespace?   "endif"   PpNewLine
      ;

lexical ConditionalSction 
     = InputSection
     | SkippedSectionPart+
     ;

lexical SkippedSectionPart 
      = SkippedCharacters?   NewLine
      | PpDirective
      ;

lexical SkippedCharacters
     = Whitespace?   ![#]   InputCharacters?
     ;

lexical PpDiagnostic
     = Whitespace?   "#"   Whitespace?   ("error" | "warning")   PpMessage
     ;

lexical PpMessage
      = NewLine
      | Whitespace   InputCharacters?   NewLine
      ;

lexical PpRegion 
      = PpStartRegion   ConditionalSection?   PpESRegion
      ;

lexical PpStartRegion 
      = Whitespace?   "#"   Whitespace?   "region"   PpMessage
      ;

lexical PpEndRegion 
      = Whitespace?   "#"   Whitespace?   "endregion"   PpMessage
      ;
      
      
lexical PpLine
     =  Whitespace?   "#"   Whitespace?   "line"   Whitespace   LineIndicator   PpNewLine
     ;
     
lexical LineIndicator 
     = DecimalDigit+   Whitespace   FileName 
     | DecimalDigit+
     | "default" 
     | "hidden"
     ;

lexical FileName 
     = "\""   FileNameCharacter+   "\""
     ;

lexical FileNameCharacter 
     = ![] \ [\"]
     ;

lexical PpPragma 
     = Whitespace?   "#"   Whitespace?   "pragma"   Whitespace   PragmaBody   PpNewLine
     ;

lexical PragmaBody 
     = PragmaWarningBody
     ;

lexical PragmaWarningBody 
     = "warning"   Whitespace   WarningAction   (Whitespace   WarningList)?
     ;

lexical WarningAction 
      = "disable"
      | "restore"
      ;

lexical WarningList 
      = DecimalDigit+
      | WarningList   Whitespace?   ","   Whitespace?   DecimalDigit+
      ;
      