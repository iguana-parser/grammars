module java::\lexical::ContextAware

//----------------------------------------------------------------------------------------------------------------
// Lexical Definititions
//----------------------------------------------------------------------------------------------------------------

token UnicodeInputCharacter 
    = UnicodeEscape
    | RawInputCharacter
    ;

token UnicodeEscape 
    = [\\] [u]+ HexDigit HexDigit HexDigit HexDigit
    ;
 
token RawInputCharacter 
    = ![\\]
    | [\\] !>> [\\ u]
    | [\\][\\]   // Java Language Specification §3.3  
    ;

token InputCharacter 
	  = ![\\ \n \r]      // UnicodeInputCharacter but not CR or LF 
    | [\\] !>> [\\ u]
    | [\\] [\\]
    | UnicodeEscape
	  | [\a00]           // to match zero        
    ;

//----------------------------------------------------------------------------------------------------------------

token Layout 
    = (WhiteSpace | Comment)*
    ;

token WhiteSpace 
    = [\ \t \f \r \n \a1a]  // to match SUB in the end, but we need a better solution for this when we added better
                              // suuport for layout
    ;
      
token LineTerminator
    = [\r \n]
    ;
     
//----------------------------------------------------------------------------------------------------------------
    
token Comment 
    = TraditionalComment
    | EndOfLineComment
    ;

token CommentChar 
    = ![\\ *]      // UnicodeInputCharacter but not CR or LF 
    | [\\] !>> [\\ u]
    | [\\] [\\]
    | [*] !>> [/]
    | UnicodeEscape
    | [\a00]           // to match zero        
    ;    

token TraditionalComment 
    = "/*" CommentChar* "*/" 
    ;

token EndOfLineComment 
    = "//" InputCharacter*
    ;
    
//----------------------------------------------------------------------------------------------------------------      

lexical Identifier 
      = [$ A-Z _ a-z] !<< IdentifierChars \Keyword \BooleanLiteral \NullLiteral
      ;

token IdentifierChars 
    = JavaLetter (JavaLetter | JavaLetterOrDigit)*
    ;

token JavaLetter 
    = [A-Za-z$_]
    ;

token JavaLetterOrDigit 
    = [A-Za-z$_0-9]
    ;

//----------------------------------------------------------------------------------------------------------------      

keyword Keyword 
      = "abstract"
      | "continue"
      | "for"
      | "new"
      | "switch"
      | "assert"
      | "default"
      | "if"
      | "package"
      | "synchronized"
      | "boolean"
      | "do"
      | "goto"
      | "private"
      | "this"
      | "break"
      | "double"
      | "implements"
      | "protected"
      | "throw"
      | "byte"
      | "else"
      | "import"
      | "public"
      | "throws"
      | "case"
      | "enum"
      | "instanceof"
      | "return"
      | "transient"
      | "catch"
      | "extends"
      | "int"
      | "short"
      | "try"
      | "char"
      | "final"
      | "interface"
      | "static"
      | "void"
      | "class"
      | "finally"
      | "long"
      | "strictfp"
      | "volatile"
      | "const"
      | "float"
      | "native"
      | "super"
      | "while"
      ;
          
//----------------------------------------------------------------------------------------------------------------

lexical Literal 
      = IntegerLiteral
      | FloatingPointLiteral
      | BooleanLiteral
      | CharacterLiteral
      | StringLiteral
      | NullLiteral
      ; 
    
lexical IntegerLiteral 
      = DecimalIntegerLiteral !>> [.]
      | HexIntegerLiteral !>> [.]
      | OctalIntegerLiteral
      | BinaryIntegerLiteral
      ; 
    
lexical FloatingPointLiteral 
      = DecimalFloatingPointLiteral
      | HexadecimalFloatingPointLiteral
      ;     

lexical DecimalIntegerLiteral 
      = DecimalNumeral IntegerTypeSuffix?
      ;

lexical HexIntegerLiteral 
      = HexNumeral IntegerTypeSuffix?
      ;

lexical OctalIntegerLiteral 
      = OctalNumeral IntegerTypeSuffix?
      ;

lexical BinaryIntegerLiteral 
      = BinaryNumeral IntegerTypeSuffix?
      ;

token IntegerTypeSuffix 
      = [l] | [L];
    
//----------------------------------------------------------------------------------------------------------------
    
token DecimalNumeral 
    = [0]
    | NonZeroDigit Digits?
    | NonZeroDigit [_]+ Digits
    ; 

token Digits 
    = Digit
    | Digit DigitOrUnderscore* Digit
    ; 

token Digit 
     = [0]
     | NonZeroDigit
     ;

token NonZeroDigit
    = [1-9]
    ;

token DigitOrUnderscore 
    = Digit
    | [_]
    ;

//----------------------------------------------------------------------------------------------------------------

token HexNumeral 
    = [0] [x] HexDigits
    | [0] [X] HexDigits
    ;

token HexDigits 
    = HexDigit
    | HexDigit HexDigitOrUnderscore* HexDigit; 

token HexDigit = [0-9 a-f A-F];

token HexDigitOrUnderscore 
    = HexDigit
    | [_]
    ;
    
//----------------------------------------------------------------------------------------------------------------    
    
token OctalNumeral 
    = [0] OctalDigits
    | [0] [_]+ OctalDigits
    ;

token OctalDigits 
    = OctalDigit
    | OctalDigit OctalDigitOrUnderscore* OctalDigit 
    ;

token OctalDigit 
    = [0-7]
    ;

token OctalDigitOrUnderscore 
    = OctalDigit
    | [_]
    ;
    
//----------------------------------------------------------------------------------------------------------------        
    
token BinaryNumeral 
    = [0] [b] BinaryDigits 
    | [0] [B] BinaryDigits
    ;

token BinaryDigits 
    = BinaryDigit 
    | BinaryDigit BinaryDigitOrUnderscore* BinaryDigit
    ;

token BinaryDigit 
    = [0-1]
    ; 

token BinaryDigitOrUnderscore
    = BinaryDigit
    | [_]
    ;
    
//----------------------------------------------------------------------------------------------------------------        


token DecimalFloatingPointLiteral 
    = Digits [.] Digits? ExponentPart? FloatTypeSuffix?
    | [.] Digits ExponentPart? FloatTypeSuffix?
    | Digits ExponentPart
    | Digits FloatTypeSuffix
    | Digits ExponentPart FloatTypeSuffix
    ;

token ExponentPart 
    = ExponentIndicator SignedInteger
    ;

token ExponentIndicator 
    = [e E]
    ;

token SignedInteger 
    = Sign? Digits
    ;

token Sign 
    = [+ \-]
    ;

token FloatTypeSuffix 
    = [f F d D]
    ;     
    
//----------------------------------------------------------------------------------------------------------------

token HexadecimalFloatingPointLiteral 
    =  HexSignificand BinaryExponent FloatTypeSuffix?
    ;

token HexSignificand 
    = HexNumeral
    | HexNumeral [.]
    | [0] [x] HexDigits? [.] HexDigits
    | [0] [X] HexDigits? [.] HexDigits
    ;

token BinaryExponent 
    = BinaryExponentIndicator SignedInteger
    ;

token BinaryExponentIndicator 
    = [p P]
    ;

//----------------------------------------------------------------------------------------------------------------

token BooleanLiteral 
     = "true" 
     | "false"
     ;

token CharacterLiteral 
    = [\'] SingleCharacter [\']
    | [\'] EscapeSequence [\']
    ;

token SingleCharacter 
    = ![\\ \n \r \'] 
    | [\\] !>> [\\ u]
    | [\\] [\\]
    | UnicodeEscape
    | [\a00]  
    ;

token StringLiteral 
    = [\"] StringCharacter* [\"]
    ;

token StringCharacter 
    = ![\\ \n \r \"] 
    | [\\] !>> [\\ u]
    | [\\] [\\]
    | UnicodeEscape
    | [\a00] 
    | EscapeSequence
    ;
        
token EscapeSequence 
    = Backslash [b]                 /* \u0008: backspace BS */
    | Backslash [t]                 /* \u0009: horizontal tab HT */
    | Backslash [n]                 /* \u000a: linefeed LF */
    | Backslash [f]                 /* \u000c: form feed FF */
    | Backslash [r]                 /* \u000d: carriage return CR */
    | Backslash [\"]                /* \u0022: double quote " */
    | Backslash [\']                /* \u0027: single quote ' */
    | [\\] [u]+ "005" [cC] [\\] [u]+ "005" [cC]   // Todo: merge it with [\\][\\]
    | OctalEscape                   /* \u0000 to \u00ff: from octal value */
    ;
     
token Backslash
    = [\\] [u]+ "005" [cC]
    | [\\]
    ;    

token OctalEscape 
    = [\\] OctalDigit
    | [\\] OctalDigit OctalDigit
    | [\\] ZeroToThree OctalDigit OctalDigit
    ;

token ZeroToThree 
    = [0-3]
    ;
    
token NullLiteral 
    = "null"
    ;

token AssignmentOperator 
    = "="
    | "+="
    | "-=" 
    | "*="
    | "/="
    | "&="
    | "|="
    | "^="
    | "%="
    | "\<\<="
    | "\>\>="
    | "\>\>\>="
    ;
