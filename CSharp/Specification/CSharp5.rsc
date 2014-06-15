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
     
syntax RankSpecifiers
     = RankSpecifier
     | RankSpecifiers   RankSpecifier
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
      