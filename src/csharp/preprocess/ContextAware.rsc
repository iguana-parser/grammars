module csharp::preprocess::ContextAware

lexical Input
     = InputSection*
     ;

lexical InputSection
     = InputSectionPart
     ;

lexical InputSectionPart
     = InputElement
     | PpDirective
     ;

lexical InputElement
     = Whitespace
     | Comment
     | Token
     ;
     
lexical Token
     = Identifier
     | Keyword
     | [A-Za-z0-9] !<< IntegerLiteral_
     | [A-Za-z0-9] !<< RealLiteral_
     | CharacterLiteral_
     | StringLiteral_
     | OperatorOrPunctuator
     ;     


layout Layout 
     = (Whitespace | Comment | DPpConditional | DPpGarbage | PpDeclaration | PpLine | PpDiagnostic | PpStartRegion | PpEndRegion  | PpPragma)* !>> [\t \n \r \f  \ ] !>> "/*" !>> "//" !>> "#"
     ; 
     
lexical Layout1 
     = (Whitespace | Comment | DPpConditional | DPpGarbage | PpDeclaration | PpLine | PpDiagnostic | PpStartRegion | PpEndRegion  | PpPragma)* !>> [\t \n \r \f  \ ] !>> "/*" !>> "//" !>> "#"
     ;       

/* 
 * Carriage return character (U+000D)
 * Line feed character (U+000A)
 * Carriage return character (U+000D) followed by line feed character (U+000A)
 * Next line character (U+0085)
 * Line separator character (U+2028)
 * Paragraph separator character (U+2029)
 */
token NewLine
    = [\r \n] //[\r \n \u0085 \u2028 \u2029]
    ;
      
// Comments

token Comment
    = SingleLineComment
    | DelimitedComment
    ;
      
token SingleLineComment
    = "//" InputCharacter*
    ;      
      
token InputCharacter 
	= ![\r \n] 		              // ![] \ [\r \n \u0085 \u2028 \u2029]    // Any Unicode character Except NewLine
	| [\a00]                    // to match zero        
    ;
      
token DelimitedComment
    = "/*" DelimitedCommentSection* [*]+ "/"
    ;

token DelimitedCommentSection
    = "/"
    | [*]*  NotSlashOrAsterisk
    ; 

token NotSlashOrAsterisk
    = ![/ *]
    ;
      
/* 
 * Any character with Unicode class Zs
 * Horizontal tab character (U+0009)
 * Vertical tab character (U+000B)
 * Form feed character (U+000C)
 */
token Whitespace
    = [\ \t \f \r \n]+ //[\u0020 \u00A0 \u1680 \u180E \u2000-\u200A \u202F \u205F \u3000 \u0009 \u000B \u000C]
    ;
      
token WhitespaceNoNL
    = [\ \t \f]+  //[\u0020 \u00A0 \u1680 \u180E \u2000-\u200A \u202F \u205F \u3000 \u0009 \u000B \u000C]
    ;      
       
// B.1.5 Unicode character escape sequences       
 
token UnicodeEscapeSequence
    = "\\u"   HexDigit   HexDigit   HexDigit   HexDigit   
    | "\\U"   HexDigit   HexDigit   HexDigit   HexDigit   HexDigit   HexDigit   HexDigit   HexDigit
    ;
      
// Identifiers      
      
lexical Identifier
     = [A-Z _ a-z] !<< IdentifierOrKeyword \ Keyword
     | "@"  IdentifierOrKeyword
     ;
      
      
token IdentifierOrKeyword
    = IdentifierStartCharacter IdentifierPartCharacter*
    ;
      
token IdentifierStartCharacter
    = LetterCharacter
    | "_"
    ;

token IdentifierPartCharacter
    = LetterCharacter
    | DecimalDigitCharacter
    | "_"
      //| ConnectingCharacter
      //| CombiningCharacter
      //| FormattingCharacter
    ;
     
token LetterCharacter
    = [a-zA-Z] //Lu | Ll | Lt | Lm | Lo | Nl
    ; 
     
//lexical CombiningCharacter
//      = Mn | Mc
//      ;
      
token DecimalDigitCharacter
     = [0-9] //Nd
     ;
       
//lexical ConnectingCharacter  
//      = Pc
//      ;      
//      
//lexical FormattingCharacter  
//      = Cf
//      ;
      
      
// Keywords      
  
token Keyword  
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
      //| "async"
      //| "await"
      ;

// Literals
      
lexical Literal_
     = BooleanLiteral_
     | [A-Za-z0-9] !<< IntegerLiteral_
     | [A-Za-z0-9] !<< RealLiteral_
     | CharacterLiteral_
     | StringLiteral_
     | InterpolatedString
     | NullLiteral_
     ;
     
token BooleanLiteral_
    = "true"
    | "false"
    ;
     
lexical IntegerLiteral_
     = DecimalIntegerLiteral
     | HexadecimalIntegerLiteral
     ;
     
token DecimalIntegerLiteral
    = DecimalDigit+ 
    | DecimalDigit+ IntegerTypeSuffix
    ;
     
token DecimalDigit
    = [0-9]
    ;     
     
token IntegerTypeSuffix
    = "U" | "u" | "L" | "l" | "UL" | "Ul" | "uL" | "ul" | "LU" | "Lu" | "lU" | "lu"
    ;
      
token HexadecimalIntegerLiteral 
    = [0][xX]   HexDigit+
    | [0][xX]   HexDigit+ IntegerTypeSuffix
    ;      
      
token HexDigit
    = [0-9  A-F  a-f]
    ;
      
token RealLiteral_
     = DecimalDigit+  "."   DecimalDigit+   ExponentPart?   RealTypeSuffix
     | DecimalDigit+  "."   DecimalDigit+  ExponentPart?
     | "."  DecimalDigit+ ExponentPart?
     | "."  DecimalDigit+  ExponentPart?   RealTypeSuffix
     | DecimalDigit+   ExponentPart   RealTypeSuffix?
     | DecimalDigit+   RealTypeSuffix
     ;
            
token  ExponentPart
    = [eE]   Sign?   DecimalDigit+
    ;
      
token Sign 
    = [+  \-]
    ;

token RealTypeSuffix 
    = [F  f  D  d  M  m]
    ;
      
token CharacterLiteral_
    = [\']   Character   [\']
    ;
     
token Character
    = SingleCharacter
    | SimpleEscapeSequence
    | HexadecimalEscapeSequence
    | UnicodeEscapeSequence
    ; 

token SingleCharacter
    = ![\' \\ \r \n \u0085 \u2028 \u2029]
    ;
      
token SimpleEscapeSequence
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
      
token HexadecimalEscapeSequence
    = "\\x"   HexDigit
    | "\\x"   HexDigit   HexDigit
    | "\\x"   HexDigit   HexDigit   HexDigit
    | "\\x"   HexDigit   HexDigit   HexDigit    HexDigit
    ;
     
token StringLiteral_
    = RegularStringLiteral
    | VerbatimStringLiteral
    ;    
      
token RegularStringLiteral
    = [\"]   RegularStringLiteralCharacter*   [\"]
    ;
      
token RegularStringLiteralCharacter
    = SingleRegularStringLiteralCharacter
    | SimpleEscapeSequence
    | HexadecimalEscapeSequence
    | UnicodeEscapeSequence
    ;
     
token SingleRegularStringLiteralCharacter
    = ![\" \\  \r \n \u0085 \u2028 \u2029]
    ;

token VerbatimStringLiteral 
    = "@" [\"]   VerbatimStringLiteralCharacter*   [\"]
    ;

token VerbatimStringLiteralCharacter
    = SingleVerbatimStringLiteralCharacter
    | QuoteEscapeSequence
    ;

token SingleVerbatimStringLiteralCharacter
    = ![\"]
    ;

token QuoteEscapeSequence
    = [\"][\"]
    ;

token NullLiteral_
    = "null"
    ;
    
lexical InterpolatedString
    = "$" "@"? [\"] [\"]
    | "$" "@"? [\"] InterpolatedStringLiteralCharacters [\"]
    ;
        
lexical InterpolatedStringLiteralCharacters
    = InterpolatedStringLiteralPart+
    ;
    
lexical InterpolatedStringLiteralPart
    = SingleInterpolatedStringLiteralCharacter
    | InterpolatedStringEscapeSequence
    | Interpolation
    ;

token SingleInterpolatedStringLiteralCharacter    
    = ![\" { }]
    ;    
    
token InterpolatedStringEscapeSequence
    = [\"][\"]
    | [\\][\"]
    | [{][{]
    | [\\][{]
    | [}][}]
    | [\\][}]
    ;
    
lexical Interpolation
    = "{" InterpolationContents "}"
    ;
    
lexical InterpolationContents
   = BalancedText
   | BalancedText ":" InterpolationFormat
   ;

lexical BalancedText
    = BalancedTextPart+
    ;

lexical BalancedTextPart
    = ![@ \" $ ( \[ {]
    | VerbatimStringLiteral
    | "@" IdentifierOrKeyword
    | RegularStringLiteral
    | InterpolatedString
    | "(" BalancedText ")"
    | "[" BalancedText "]"
    | "{" BalancedText "}"
    | DelimitedComment
    | SingleLineComment
    | Expression
    ;

token InterpolationFormat
    = LiteralInterpolationFormat
    ;

token LiteralInterpolationFormat
    = InterpolationFormatPart+
    ;

token InterpolationFormatPart
    = [\" : { } \r \n]
    ;

      
// Operators and punctuators   
   
lexical OperatorOrPunctuator
     = "{"
     | "}"
     | "["
     | "]"
     | "("
     | ")"
     | "." !>> [0-9]
     | ","
     | ":"  !>> [:]
     | ";"
     | "+"  !>> [+ =]
     | "-"  !>> [\- =]
     | "*"  !>> [= *]
     | "/"  !>> [/ = *]
     | "%"  !>> [=]
     | "&"  !>> [& =]
     | "|"  !>> [= |]
     | "^"  !>> [=]
     | "!"  !>> [=]
     | "~"
     | "="  !>> [= \>]
     | "\<" !>> [= \<]
     | "\>" !>> [=]
     | "?"  !>> [?]
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
     | "&=" !>> [:]
     | "|="
     | "^="
     | "\<\<" !>> [=]
     | "\<\<="
     | "=\>"
     ;
      
token RightShift
    = "\>\>"
    ;
      
token RightShiftAssignment
    = "\>\>="
    ;
     
// Conditional directives with evalutation

lexical DPpConditional 
      = DPpIfSection
      ;

lexical DPpIfSection 
      = "#"   Whitespace?   "if"   Whitespace   PpExpression exp if(ppLookup(exp)) Layout1 else (SkippedSection (DPpElifSection | DPpElseSection | PpEndif))
      ;

lexical DPpElifSection
      = "#"   Whitespace?   "elif"   Whitespace   PpExpression exp if(ppLookup(exp)) Layout1 else (SkippedSection (DPpElifSection | DPpElseSection | PpEndif))
      ;

lexical DPpElseSection
      = "#"   Whitespace?   "else"   Layout1
      ;
      
lexical DPpGarbage
      = DPpElif* DPpElse? "#"   Whitespace?   "endif"
      ;
      
lexical DPpElif 
      = "#"   Whitespace?   "elif" SkippedSection
      ;
      
lexical DPpElse 
      = "#"   Whitespace?   "else" SkippedSection
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
      = IdentifierOrKeyword !>> [0-9 A-Z _ a-z] \ "true" \ "false"
      ;
      
lexical PpExpression
      = PpOrExpression
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
     | ConditionalSymbol
     | "("   Whitespace?   PpExpression   Whitespace?  ")"
     ;
     
lexical PpDeclaration
      = "#"   Whitespace?   "define"  Whitespace   ConditionalSymbol sym do ppDeclare(sym, true);  PpNewLine
      | "#"   Whitespace?   "undef"   Whitespace   ConditionalSymbol sym do ppDeclare(sym, false); PpNewLine
      ;
      
lexical PpNewLine 
     = WhitespaceNoNL? SingleLineComment? NewLine
     | WhitespaceNoNL? SingleLineComment? $$
     ;

lexical PpConditional 
      = PpIfSection   PpElifSection*   PpElseSection?   PpEndif
      ;

lexical PpIfSection 
      = "#"   Whitespace?   "if"   Whitespace   PpExpression   PpNewLine   SkippedSection?
      ;

lexical PpElifSection
      = "#"   Whitespace?   "elif"   Whitespace   PpExpression   PpNewLine   SkippedSection?
      ;

lexical PpElseSection
      = "#"   Whitespace?   "else"   PpNewLine   SkippedSection?
      ;

lexical PpEndif
      = "#"   Whitespace?   "endif"   PpNewLine
      ;

lexical SkippedSection 
     = SkippedSectionPart+
     ;

lexical SkippedSectionPart 
      = SkippedCharacters
      | WhitespaceNoNL
      | SingleLineComment
      | PpDirective
      | NewLine
      ;

lexical SkippedCharacters
     = ![# \ \t \f \r \n /]
     | [/] !>> [/] 
     ;
     
lexical PpDiagnostic
     = "#"   Whitespace?   ("error" | "warning")   PpMessage
     ;

lexical PpMessage
      = InputCharacter*   NewLine
      ;

lexical PpRegion 
      = PpStartRegion 	InputSection*  PpEndRegion
      ;

lexical PpStartRegion 
      = "#"   Whitespace?   "region"   PpMessage
      ;

lexical PpEndRegion 
      = "#"   Whitespace?   "endregion"   PpMessage
      ;
      
      
lexical PpLine
    =  "#"   Whitespace?   "line"   Whitespace   LineIndicator   PpNewLine
    ;
     
token LineIndicator 
    = DecimalDigit+   Whitespace   FileName 
    | DecimalDigit+
    | "default" 
    | "hidden"
    ;

token FileName 
    = "\""   FileNameCharacter+   "\""
    ;

token FileNameCharacter 
    = ![\"]
    ;

lexical PpPragma 
     = "#"   Whitespace?   "pragma"   Whitespace   PragmaBody
     ;

lexical PragmaBody 
     = PragmaWarningBody
     | PragmaChecksumBody
     ;

lexical PragmaWarningBody 
     = "warning"   Whitespace   WarningAction   NewLine
     | "warning"   Whitespace   WarningAction   Whitespace   WarningList
     ;
     
token PragmaChecksumBody
    = "checksum" Whitespace StringLiteral_ Whitespace StringLiteral_ Whitespace StringLiteral_
    ;      

token WarningAction 
    = "disable"
    | "restore"
    ;

lexical WarningList 
     = DecimalDigit+
     | WarningList   Whitespace?   ","   Whitespace?   DecimalDigit+
     ;

    