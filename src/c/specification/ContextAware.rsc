

// Lexical elements

module c::specification::ContextAware

lexical Token
      = Keyword
      | Identifier 
      | Constant 
      | StringLiteral 
      | Punctuator
      ;

lexical PreprocessingToken 
      = HeaderName
      | Identifier
      | PpNumber
      | CharacterConstant
      | StringLiteral
      | Punctuator
      // each non-white-space character that cannot be one of the above      
      ;

// Keywords

lexical Keyword
      = "auto"
      | "break"
      | "case"
      | "char"
      | "const"
      | "continue"
      | "default"
      | "do"
      | "double"
      | "else"
      | "enum"
      | "extern"
      | "float"
      | "for"
      | "goto"
      | "if"
      | "inline"
      | "int"
      | "long"
      | "register"
      | "restrict"
      | "return"
      | "short"
      | "signed"
      | "sizeof"
      | "static"
      | "struct"
      | "switch"
      | "typedef"
      | "union"
      | "unsigned"
      | "void"
      | "volatile"
      | "while"
      | "_Alignas"
      | "_Alignof"
      | "_Atomic"
      | "_Bool"
      | "_Complex"
      | "_Generic"
      | "_Imaginary"
      | "_Noreturn"
      | "_Static_assert"
      | "_Thread_local"
      ;

// Identifiers

//lexical Identifier
//      = IdentifierNonDigit
//      | Identifier IdentifierNonDigit
//      | Identifier Digit
//      ;

lexical Identifier
      = [_ a-z A-Z 0-9] !<< IdentifierChars \ Keyword
      ;

token IdentifierChars
    = IdentifierNonDigit (IdentifierNonDigit | Digit)*
    ;

token IdentifierNonDigit
    = NonDigit
    | UniversalCharacterName
    // other implementation-defined characters
    ;

token NonDigit
      = [_a-zA-Z]
      ;

token Digit
      = [0-9]
      ;

// Universal character names

token UniversalCharacterName
      = "\\u" HexQuad
      | "\\U" HexQuad HexQuad
      ;

token HexQuad
      = HexadecimalDigit HexadecimalDigit HexadecimalDigit HexadecimalDigit
      ;

// Constants

token Constant
      = IntegerConstant
      | FloatingConstant
      //| EnumerationConstant  (See 1)
      | CharacterConstant
      ;

token IntegerConstant
      = DecimalConstant IntegerSuffix?
      | OctalConstant IntegerSuffix?
      | HexadecimalConstant IntegerSuffix?
      ;

token DecimalConstant
      = NonZeroDigit
      | DecimalConstant Digit
      ;

token OctalConstant
      = "0"
      | OctalConstant OctalDigit
      ;

token HexadecimalConstant
      = HexadecimalPrefix HexadecimalDigit 
      | HexadecimalConstant HexadecimalDigit
      ;

token HexadecimalPrefix
      = "0x" 
      | "0X"
      ;

token NonZeroDigit 
      = [1-9]
      ;

token OctalDigit
      = [0-7]
      ;

token HexadecimalDigit
      = [0-9 a-f A-F]
      ;

token IntegerSuffix
      = UnsignedSuffix LongSuffix?
      | UnsignedSuffix LongLongSuffix 
      | LongSuffix UnsignedSuffix?
      | LongLongSuffix UnsignedSuffix?
      ;

token UnsignedSuffix
      = [uU]
      ;

token LongSuffix
      = [lL]
      ;

token LongLongSuffix
      = "ll"
      | "LL"
      ;

token FloatingConstant
      = DecimalFloatingConstant 
      | HexadecimalFloatingConstant
      ;

token DecimalFloatingConstant
      = FractionalConstant ExponentPart? FloatingSuffix?
      | DigitSequence ExponentPart FloatingSuffix?
      ;
      
token HexadecimalFloatingConstant
      = HexadecimalPrefix HexadecimalFractionalConstant BinaryExponentPart FloatingSuffix?
      | HexadecimalPrefix HexadecimalDigitSequence BinaryExponentPart FloatingSuffix?
      ;      

token FractionalConstant
     = DigitSequence? "." DigitSequence 
     | DigitSequence "."
     ;

token ExponentPart
     = "e" Sign? DigitSequence
     | "E" Sign? DigitSequence
     ;

token Sign
      = [+ \-]
      ;

token DigitSequence
      = Digit+
      ;

token HexadecimalFractionalConstant
      = HexadecimalDigitSequence? "." HexadecimalDigitSequence 
      | HexadecimalDigitSequence "."
      ;

token BinaryExponentPart
      = "p" Sign? DigitSequence 
      | "P" Sign? DigitSequence
      ;

token HexadecimalDigitSequence
      = HexadecimalDigit+
      ;

token FloatingSuffix
      = [flFL]
      ;

token EnumerationConstant
      = Identifier
      ;

token CharacterConstant
      = "\'" CCharSequence "\'"
      | "L\'" CCharSequence "\'" 
      | "u\'" CCharSequence "\'" 
      | "U\'" CCharSequence "\'"
      ;

token CCharSequence
      = CChar+
      ;

token CChar
     = ![\' \\ \n \r]
     | EscapeSequence
     ;

token EscapeSequence
      = SimpleEscapeSequence
      | OctalEscapeSequence
      | HexadecimalEscapeSequence
      | UniversalCharacterName
      ;

token SimpleEscapeSequence
      = "\\" "\'" 
      | "\\" "\"" 
      | "\\" "?"
      | "\\" "\\"
      | "\\" "a"  
      | "\\" "b"  
      | "\\" "f" 
      | "\\" "n"
      | "\\" "r"
      | "\\" "t"  
      | "\\" "v"
      ;

token OctalEscapeSequence
      = "\\" OctalDigit
      | "\\" OctalDigit OctalDigit
      | "\\" OctalDigit OctalDigit OctalDigit
      ;

token HexadecimalEscapeSequence
      = ("\\x"? HexadecimalDigit)+
      ;

// String literals

token StringLiteral
      = EncodingPrefix? "\"" SCharSequence? "\""
      ;

token EncodingPrefix
     = "u8"
     | "u" 
     | "U" 
     | "L"
     ;

token SCharSequence
      = SChar+
      ;

token SChar
      = ![\" \\ \n \r]
      | EscapeSequence
      ;

// Punctuators

token Punctuator
      = "["
      | "]"
      | "("
      | ")"
      | "{"
      | "}" 
      | "."
      | "-\>"
      | "++"  
      | "--"  
      | "&"  
      | "*"  
      | "+"  
      | "-"  
      | "~"  
      | "!"
      | "/"  
      | "%"  
      | "\<\<"  
      | "\>\>"  
      | "\<"  
      | "\>"
      | "\<="
      | "\>="  
      | "=="  
      | "!="  
      | "^"  
      | "|"  
      | "&&"
      | "||"
      | "?"
      | ":"  
      | ";"  
      | "..."
      | "="  
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
      | "," 
      | "#"
      | "##"
      | "\<:"
      | ":\>"
      | "\<%"
      | "%\>"  
      | "%:"  
      | "%:%:"
      ;

// Header names

token HeaderName
     = "\<" HCharSequence "\>"
     | "\"" QCharSequence "\""
     ;

token HCharSequence
      = HChar+
      ;

token HChar
      = ![\n \r \>]
      ;

token QCharSequence
      = QChar+
      ;

token QChar
      = ![\n \r \"]
      ;

// Preprocessing numbers

lexical PpNumber
      = Digit
      | "." Digit
      | PpNumber Digit
      | PpNumber IdentifierNonDigit
      | PpNumber "e" Sign
      | PpNumber "E" Sign
      | PpNumber "p" Sign
      | PpNumber "P" Sign 
      | PpNumber "."
      ;

// Preprocessing directives

lexical PreprocessingFile
      = Group?
      ;

lexical Group
      = GroupPart
      | Group GroupPart
      ;

lexical GroupPart
      = IfSection
      | ControlLine TextLine
      | "#" !>> "#" NonDirective
      ;

lexical IfSection
      = IfGroup ElifGroups? ElseGroup? EndifLine
      ;

lexical IfGroup
      = "#" "if"     ConstantExpression NewLine Group?
      | "#" "ifdef"  Identifier NewLine Group? 
      | "#" "ifndef" Identifier NewLine Group?
      ;

lexical ElifGroups
      = ElifGroup
      | ElifGroups ElifGroup
      ;

lexical ElifGroup
      = "#" "elif" ConstantExpression NewLine Group?
      ;

lexical ElseGroup
      = "#" "else" NewLine Group?
      ;

lexical EndifLine
      = "#" "endif" NewLine
      ;

lexical ControlLine
      = "#" "include" PpTokens NewLine
      | "#" "define"  Identifier ReplacementList NewLine
      | "#" "define"  Identifier LParen IdentifierList? ")" ReplacementList NewLine
      | "#" "define"  Identifier LParen "..." ")" ReplacementList NewLine
      | "#" "define"  Identifier LParen IdentifierList "," "..." ")" ReplacementList NewLine
      | "#" "undef"   Identifier NewLine
      | "#" "line"    PpTokens NewLine
      | "#" "error"   PpTokens? NewLine
      | "#" "pragma"  PpTokens? NewLine 
      | "#"           NewLine  
      ;

lexical TextLine
      = PpTokens? NewLine
      ;

lexical NonDirective
      = PpTokens NewLine
      ;

lexical LParen
      = [\ \r \n \t \f] !<< "("
      ;

lexical ReplacementList
      = PpTokens?
      ;

lexical PpTokens
      = PreprocessingToken
      | PpTokens PreprocessingToken
      ;

// Layout

token NewLine
    = [\r \n]
    ;
      
token WhiteSpace 
    = [\ \t \f \r \n]
    ;     
      
token Comment
    = SingleLineComment
    | TraditionalComment
    ;      

token SingleLineComment
    = "//" ![\n\r]*
    ;

token TraditionalComment
    = "/*" ([/] | ([*]* ![/ *]))* [*]+ "/" 
    ;
      
token Layout 
     = (WhiteSpace | Comment)*
     ;

      