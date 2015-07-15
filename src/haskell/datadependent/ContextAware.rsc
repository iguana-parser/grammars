module haskell::datadependent::ContextAware

token LiteralH 
      = Integer 
      | Integer "#" 
      | Integer "##"
      | Float 
      | Float "#" 
      | Float "##"
      | Char  
	  | Char "#"      
      | String
      | String "#"
      ;

token Special 
      = "(" 
      | ")" 
      | "," 
      | ";" 
      | "["
      | "]" 
      | "`" 
      | "{" 
      | "}"
      ;
 
layout Whitespace  
     = WhiteStuff* !>> [\n \r \t \ \u000B] !>> "{-" !>> "--"
     ;

lexical WhiteStuff  
      = WhiteChar 
      | Comment 
      | NComment
      ;

token WhiteChar   
      = [\n \r \t \ \u000B]*
      ;

token NewLine
      = [\r \n]
      ;
      
token Space
      = " "
      ;          
      
token Tab
      = [\t]
      ;        
 
token Comment 
      = Dashes (Any \ Symbol Any*)? "\n"
      ;

token Dashes  
      =   "--" "-"*
      ;

lexical NComment    
      = "{-" (![{\-] | NComment | "-" !>> [}] | "{" !>> [\-])* "-}"
      ;

token Any 
      = Graphic 
      | Space 
      | Tab
      ;

token Graphic 
      = Small 
      | Large 
      | Symbol 
      | Digit 
      | Special 
      | "\"" 
      | "\'"
 	  ;
 	  
token GraphicNoQuoate 
      = Small 
      | Large 
      | Symbol 
      | Digit 
      | Special 
 	  ; 	  
 
token Small   
      = AscSmall 
//     | UniSmall 
      | "_"
      ;

token AscSmall    
     =  [a-z]
     ;

//lexical UniSmall    
//      =   any Unicode lowercase letter;
 
token Large   
      = AscLarge 
//    | UniLarge
      ;

token AscLarge    
      = [A-Z]
      ;

//lexical uniLarge    =   any uppercase or titlecase Unicode letter

token Symbol  
      = AscSymbol 
//    | UniSymbol \ Special \ "_" \ "\"" \ "'"
      ;
 
token AscSymbol   
      = "!" 
      | "#" 
      | "$" 
      | "%" 
      | "&" 
      | "+"
      | "*" 
      | "." 
      | "/" 
      | "\<" 
      | "=" 
      | "\>" 
      | "?" 
      | "@"
      | "\\" 
      | "^" 
      | "|" 
      | "-" 
      | "~" 
      | ":"
      ;

// lexical uniSymbol   =   any Unicode symbol or punctuation

token Digit   
      = AscDigit 
//    | UniDigit
      ;

token AscDigit    
     =  [0-9]
     ;

// lexical uniDigit    =   any Unicode decimal digit

token OctIt   
      =   [0-7]
      ;

token HexIt   
      =  Digit 
      |  [A-Fa-f]
      ;
      
lexical QuasiVarId
     =  [a-zA-Z0-9_] !<< "\'" "\'"? Small (Small | Large | Digit)* !>> [a-zA-Z0-9_\'] 
     ;
      
lexical QuasiConId
     =  [a-zA-Z0-9_] !<< "\'" "\'"? Large (Small | Large | Digit)* !>> [a-zA-Z0-9_\'] 
     ;
 
lexical VarId   
      = [a-zA-Z0-9_] !<< (Small (Small | Large | Digit | "\'")*) !>> [a-zA-Z0-9_\'] \ ReservedId !>> "#"
      | [a-zA-Z0-9_] !<< (Small (Small | Large | Digit | "\'")*) !>> [a-zA-Z0-9_\'] \ ReservedId "#"
      | QuasiVarId
      ;

lexical ConId   
      = [a-zA-Z0-9_] !<< Large (Small | Large | Digit | "\'")* !>> [a-zA-Z0-9_\'] !>> "#"
      | [a-zA-Z0-9_] !<< Large (Small | Large | Digit | "\'")* !>> [a-zA-Z0-9_\'] "#"
      | QuasiConId
      ;

lexical ReservedId  
      = "case" 
      | "class" 
      | "data" 
      | "default" 
      | "deriving" 
      | "do" 
      | "else"
      | "foreign" 
      | "if" 
      | "import" 
      | "in" 
      | "infix" 
      | "infixl"
      | "infixr" 
      | "instance" 
      | "let" 
      | "module" 
      | "newtype" 
      | "of"
      | "then" 
      | "type" 
      | "where" 
      | "_"
      ;
 
lexical VarSym  
      = ( Symbol \ ":" Symbol* ) !>> [! # $ % & . + * / \< = \> ? @ \\ ^ | \- ~ :] \ ReservedOp \ Dashes
      ;

lexical ConSym  
      =  ( ":" Symbol+) \ "::"
      ;

lexical ReservedOp  
      = ".."
      | ":" 
      | "::" 
      | "=" 
      | "\\" 
      | "|" 
      | "\<-" 
      | "-\>" 
      | "@" 
      | "~" 
      | "=\>"
      | "-\<"
	  | "\>-"
	  | "-\<\<" 
	  | "\>\>-" 
      ;

lexical TyVar
     =  VarId  \ "family" \ "forall"
     ;

lexical TyCon   
      = ConId
      ;

lexical TyCls   
      = ConId
      ;

lexical ModId   
      = (ConId ".")* ConId
      ;
 
lexical QVarId  
      = (ModId ".")? VarId
      ;

lexical QConId  
      = (ModId ".")? ConId
      ;

lexical QTyCon  
      = (ModId ".")? TyCon
      ;

lexical QTyCls  
      = (ModId ".")? TyCls
      ;

lexical QVarSym   
      = ( ModId "." )? VarSym
      ;

lexical QConSym 
      = (ModId ".")? ConSym
      ;
 
token Decimal 
      = Digit Digit*
      ;

token Octal   
     =  OctIt OctIt*
     ;

token Hexadecimal 
      = HexIt HexIt*
      ;
 
token Integer 
      = Decimal
      | "0o" Octal 
      | "0O" Octal
      | "0x" Hexadecimal 
      | "0X" Hexadecimal
      ;

token Float   
      = Decimal "." Decimal Exponent?
      | Decimal Exponent
      ;

token Exponent    
    = [eE] [+\-]? Decimal
     ;
 
token Char    
    = "\'" (GraphicNoQuoate | Space | EscapeNoAnd) "\'"
    ;

token String  
    =  "\"" (GraphicNoQuoate | Space | Escape | Gap)* "\""
    ;

token EscapeNoAnd
    = "\\" ( CharEscNoAnd | Ascii | Decimal | ("o" Octal) | ("x" Hexadecimal) )
    ;

token Escape  
    = "\\" ( CharEsc | Ascii | Decimal | ("o" Octal) | ("x" Hexadecimal) )
    ;
      
token CharEscNoAnd
    = "a" 
    | "b" 
    | "f" 
    | "n" 
    | "r" 
    | "t" 
    | "v" 
    | "\\" 
    | "\"" 
    | "\'" 
    ;       
      
token CharEsc 
      = "a" 
      | "b" 
      | "f" 
      | "n" 
      | "r" 
      | "t" 
      | "v" 
      | "\\" 
      | "\"" 
      | "\'" 
      | "&"
      ;

token Ascii   
      = "^" Cntrl 
      | "NUL" 
      | "SOH" 
      | "STX" 
      | "ETX" 
      | "EOT" 
      | "ENQ" 
      | "ACK"
      | "BEL" 
      | "BS" 
      | "HT" 
      | "LF" 
      | "VT" 
      | "FF" 
      | "CR" 
      | "SO"
      | "SI" 
      | "DLE"
      | "DC1" 
      | "DC2" 
      | "DC3" 
      | "DC4" 
      | "NAK" 
      | "SYN" 
      | "ETB" 
      | "CAN"
      | "EM" 
      | "SUB" 
      | "ESC" 
      | "FS" 
      | "GS" 
      | "RS" 
      | "US" 
      | "SP" 
      | "DEL"
      ;

token Cntrl   
      = AscLarge 
      | "@" 
      | "[" 
      | "\\" 
      | "]" 
      | "^" 
      | "_"
      ;

token Gap 
      = "\\" WhiteChar+ "\\"
      ;
      