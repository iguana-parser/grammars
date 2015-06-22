

// Lexical elements

module c::specification::Lexical

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
      // |each non-white-space character that cannot be one of the above      
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
lexical Identifier
      = IdentifierNonDigit
      | Identifier IdentifierNonDigit
      | Identifier Digit
      ;

lexical IdentifierNonDigit
      = NonDigit
      | UniversalCharacterName
      // other implementation-defined characters
      ;

lexical NonDigit
      = [_a-zA-Z]
      ;

lexical Digit
      = [0-9]
      ;

// Universal character names

lexical UniversalCharacterName
      = "\\u" HexQuad
      | "\\U" HexQuad HexQuad
      ;

lexical HexQuad
      = HexadecimalDigit HexadecimalDigit HexadecimalDigit HexadecimalDigit
      ;

// Constants

lexical Constant
      = IntegerConstant
      | FloatingConstant
      | EnumerationConstant
      | CharacterConstant
      ;

lexical IntegerConstant
      = DecimalConstant IntegerSuffix?
      | OctalConstant IntegerSuffix?
      | HexadecimalConstant IntegerSuffix?
      ;

lexical DecimalConstant
      = NonZeroDigit
      | DecimalConstant Digit
      ;

lexical OctalConstant
      = "0"
      | OctalConstant OctalDigit
      ;

lexical HexadecimalConstant
      = HexadecimalPrefix HexadecimalDigit 
      | HexadecimalConstant HexadecimalDigit
      ;

lexical HexadecimalPrefix
      = "0x" 
      | "0X"
      ;

lexical NonZeroDigit 
      = [1-9]
      ;

lexical OctalDigit
      = [0-7]
      ;

lexical HexadecimalDigit
      = [0-9 a-f A-F]
      ;

lexical IntegerSuffix
      = UnsignedSuffix LongSuffix?
      | UnsignedSuffix LongLongSuffix 
      | LongSuffix UnsignedSuffix?
      | LongLongSuffix UnsignedSuffix?
      ;

lexical UnsignedSuffix
      = [uU]
      ;

lexical LongSuffix
      = [lL]
      ;

lexical LongLongSuffix
      = "ll"
      | "LL"
      ;

lexical FloatingConstant
      = DecimalFloatingConstant 
      | HexadecimalFloatingConstant
      ;

lexical DecimalFloatingConstant
      = FractionalConstant ExponentPart? FloatingSuffix?
      | DigitSequence ExponentPart FloatingSuffix?
      ;
      
lexical HexadecimalFloatingConstant
      = HexadecimalPrefix HexadecimalFractionalConstant BinaryExponentPart FloatingSuffix?
      | HexadecimalPrefix HexadecimalDigitSequence BinaryExponentPart FloatingSuffix?
      ;      

lexical FractionalConstant
     = DigitSequence? "." DigitSequence 
     | DigitSequence "."
     ;

lexical ExponentPart
     = "e" Sign? DigitSequence
     | "E" Sign? DigitSequence
     ;

lexical Sign
      = [+ \-]
      ;

lexical DigitSequence
      = Digit
      | DigitSequence Digit
      ;

lexical HexadecimalFractionalConstant
      = HexadecimalDigitSequence? "." HexadecimalDigitSequence 
      | HexadecimalDigitSequence "."
      ;

lexical BinaryExponentPart
      = "p" Sign? DigitSequence 
      | "P" Sign? DigitSequence
      ;

lexical HexadecimalDigitSequence
      = HexadecimalDigit
      | HexadecimalDigitSequence HexadecimalDigit   
      ;

lexical FloatingSuffix
      = [flFL]
      ;

lexical EnumerationConstant
      = Identifier
      ;


lexical CharacterConstant
      = "\'" CCharSequence "\'"
      | "L\'" CCharSequence "\'" 
      | "u\'" CCharSequence "\'" 
      | "U\'" CCharSequence "\'"
      ;

lexical CCharSequence
      = CChar
      | CCharSequence CChar
      ;

lexical CChar
     = ![] \ [\' \\ \n \r]
     | EscapeSequence
     ;

lexical EscapeSequence
      = SimpleEscapeSequence
      | OctalEscapeSequence
      | HexadecimalEscapeSequence
      | UniversalCharacterName
      ;

lexical SimpleEscapeSequence
      = "\'" 
      | "\\\"" 
      | "\\?"
      | "\\\\"
      | "\\a"  
      | "\\b"  
      | "\\f" 
      | "\\n"
      | "\\r"
      | "\\t"  
      | "\\v"
      ;

lexical OctalEscapeSequence
      = "\\" OctalDigit
      | "\\" OctalDigit OctalDigit
      | "\\" OctalDigit OctalDigit OctalDigit
      ;

lexical HexadecimalEscapeSequence
      = "\\x" HexadecimalDigit
      | HexadecimalEscapeSequence HexadecimalDigit
      ;

// String literals

lexical StringLiteral
      = EncodingPrefix? "\"" SCharSequence? "\""
      ;

lexical EncodingPrefix
     = "u8"
     | "u" 
     | "U" 
     | "L"
     ;

lexical SCharSequence
      = SChar
      | SCharSequence SChar
      ;

lexical SChar
      = ![] \ [\" \\ \n \r]
      | EscapeSequence
      ;

// Punctuators

lexical Punctuator
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

lexical HeaderName
     = "\<" HCharSequence "\>"
     | "\"" QCharSequence "\""
     ;

lexical HCharSequence
      = HChar
      | HCharSequence HChar
      ;

lexical HChar
      = ![] \ [\n \r \>]
      ;

lexical QCharSequence
      = QChar
      | QCharSequence QChar
      ;

lexical QChar
      = ![] \ [\n \r \"]
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
      | "#" NonDirective
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

lexical NewLine 
      = [\r \n]
      ;
      
lexical WhiteSpace 
      = [\ \t \f \r \n] 
      ;     
      
lexical Comment
	= "//" ![\n]*
	| "/*" (![*] | [*] !>> [/])* "*/" 
	;      
      
layout Layout 
     = (WhiteSpace | Comment)* !>> [\t \n \r \f  \ ] !>> "/*" !>> "//";
      
      
      