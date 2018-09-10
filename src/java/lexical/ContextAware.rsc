module java::\lexical::ContextAware

token UnicodeEscape 
    = [\\] [u]+ HexDigit HexDigit HexDigit HexDigit
    ;
 
token Layout 
    = (WhiteSpace | Comment)*
    ;

token WhiteSpace 
    = [\ \t \f \r \n \a1a]  // to match SUB in the end, but we need a better solution for this when we added better
                              // suuport for layout
    ;
      
token Comment 
    = TraditionalComment
    | EndOfLineComment
    ;

token CommentChar 
    = [/] 
    | [*]* ![/ *]      // Modified to allow DFA compilation
    | [\a00]           // to match zero        
    ;    

token TraditionalComment 
    = "/*" CommentChar* [*]+  "/" 
    ;

token EndOfLineComment 
    = "//" ![\r \n \a00]* [\r\n\a00]
    ;
    
//----------------------------------------------------------------------------------------------------------------      

token Identifier 
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
      = integerLiteral:   IntegerLiteral !>> [.]
      | floatLiteral:     FloatingPointLiteral
      | booleanLiteral:   BooleanLiteral
      | characterLiteral: CharacterLiteral
      | stringLiteral:    StringLiteral
      | nullLiteral:      NullLiteral
      ; 
    
token IntegerLiteral 
      = DecimalIntegerLiteral
      | HexIntegerLiteral
      | OctalIntegerLiteral
      | BinaryIntegerLiteral
      ; 
    
token FloatingPointLiteral 
      = DecimalFloatingPointLiteral
      | HexadecimalFloatingPointLiteral
      ;     

token DecimalIntegerLiteral 
      = ([0] | ([1-9] (Digits? | ([_]+ Digits)))) [lL]? 
      ;

token HexIntegerLiteral
	  = [0] [xX] [0-9a-fA-F] ([0-9a-fA-F_]* [0-9a-fA-F])? [lL]?
      ;

token OctalIntegerLiteral 
      = [0] [_]* [0-7] ([0-7_]* [0-7])? [lL]?
      ;

token BinaryIntegerLiteral 
      = [0] [bB] [01] ([01_]* [01])? [lL]?
      ;

token Digits 
	= [0-9] ([0-9_]* [0-9])?
	;

token HexNumeral 
    = [0] [xX] HexDigits
    ;

token HexDigits 
    = HexDigit (HexDigitOrUnderscore* HexDigit)?
    ; 

token HexDigit = [0-9 a-f A-F];

token HexDigitOrUnderscore 
    = [0-9 a-f A-F _]
    ;
    
token OctalNumeral 
    = [0] [_]* OctalDigits
    ;

token OctalDigits 
    = OctalDigit (OctalDigitOrUnderscore* OctalDigit)?
    ;

token OctalDigit 
    = [0-7]
    ;

token OctalDigitOrUnderscore 
    = [0-7_]
    ;
    
//----------------------------------------------------------------------------------------------------------------        
    
token BinaryNumeral 
    = [0] [bB] BinaryDigits 
    ;

token BinaryDigits 
    = BinaryDigit (BinaryDigitOrUnderscore* BinaryDigit)? 
    ;

token BinaryDigit 
    = [0-1]
    ; 

token BinaryDigitOrUnderscore
    = [0-1_]
    ;
    
//----------------------------------------------------------------------------------------------------------------        


token DecimalFloatingPointLiteral 
    = ((Digits '.' Digits?) | ('.' Digits)) ExponentPart? FloatTypeSuffix?
    | Digits ((ExponentPart FloatTypeSuffix?) | FloatTypeSuffix)
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
	= [0] [xX] ((HexDigits [.]?) | (HexDigits? [.] HexDigits)) [pP] Sign? Digits FloatTypeSuffix? 
    ;

//----------------------------------------------------------------------------------------------------------------

token BooleanLiteral 
     = "true" 
     | "false"
     ;

token CharacterLiteral 
    = [\'] (SingleCharacter | EscapeSequence) [\']
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
