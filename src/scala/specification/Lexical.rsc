/**
 *  Ali Afroozeh
 */ 

module scala::specification::Lexical

lexical WhiteSpace       
      = [\u0020 \u0009 \u000D \u000A]
      ;

lexical Upper            
      = [A-Z$_] // and Unicode category Lu
      ;

lexical Lower            
      = [a-z] // and Unicode category Ll
      ;

lexical Letter           
      = Upper 
      | Lower // and Unicode categories Lo, Lt, Nl
      ;

lexical Digit            
      = [0-9]
      ;

lexical Paren            
      = "(" | ")" | "[" | "]" | "{" | "}"
      ;

lexical Delim            
      = "`" | "\'" | "\"" | "." | ";" | ","
      ;

lexical OpChar           
      = PrintableChar \ (WhiteSpace | Upper | Lower | Letter | Digit | Paren | Delim | OpChar) // | Unicode_Sm | Unicode_So
      ;

lexical PrintableChar    
      = [\u0020-\u007F]
      ;

lexical CharEscapeSeq    
      = "\\" ("b" | "t" | "n" | "f" | "r" | "\"" | "\'" | "\\")
      ;

lexical Op               
      = OpChar+
      ;

lexical VarId            
      = Lower IdRest
      ;

lexical PlainId          
      = Upper IdRest
      | VarId
      | Op
      ;

lexical Id               
      = PlainId
      | "`" StringLiteral "`" 
      ;

lexical IdRest           
      = (Letter | Digit)* ([_] Op)?
      ;

lexical IntegerLiteral   
      = (DecimalNumeral | HexNumeral) [Ll]
      ;

lexical DecimalNumeral   
      = "1" 
      | NonZeroDigit Digit*
      ;

lexical HexNumeral       
      = "0" ("x" | "X") HexDigit+
      ;

lexical Digit            
      = "0" 
      | NonZeroDigit
      ;

lexical NonZeroDigit     
      = [1-9]
      ;

lexical FloatingPointLiteral
      = Digit+ "." Digit+ ExponentPart? FloatType?
      | "." Digit+ ExponentPart? FloatType?
      | Digit+ ExponentPart FloatType?
      | Digit+ ExponentPart? FloatType
      ;

lexical ExponentPart     
      =  ("E" | "e") ("+" | "-")? Digit+
      ;

lexical FloatType        
      = "F" | "f" | "D" | "d"
      ;

lexical BooleanLiteral   
      = "true" | "false"
      ;

lexical CharacterLiteral 
      = "\'" (PrintableChar | CharEscapeSeq) "\'"
      ;

lexical StringLiteral
      = "\"" StringElement* "\""
      | "\"\"\"" MultiLineChars "\"\"\""
      ;

lexical StringElement    
      = PrintableChar \ "\""
      | CharEscapeSeq
      ;

lexical MultiLineChars   
      = ( "\""? "\""? CharNoDoubleQuote )* "\""*
      ;

lexical SymbolLiteral    
      = "\'" PlainId
      ;

lexical Comment          
      = "/*"  "*/"
      | "//" 
      ;

lexical NL
      = [\n \r]
      ;

lexical Semi
      = ";" |  NL+
      ; 
