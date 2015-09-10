module ocaml::specification::ContextAware

// Lexical

lexical Ident 
     = LowercaseIdentifier 
     | CapitalizedIdentifier
     ; 

// underscore is considered a lower case identifier
lexical LowercaseIdentifier 
      = [a-zA-Z_0-9] !<< LowercaseIdentifierChars \ Keywords
      ;
      
token LowercaseIdentifierChars
    = [a-z_] [A-Za-z0-9_\']*
    ;

lexical CapitalizedIdentifier 
      = [a-zA-Z_0-9] !<< CapitalizedIdentifierChars \ Keywords
      ;
      
token CapitalizedIdentifierChars
    = [A-Z] [A-Za-z0-9_\']*
    ;

token Int32Literal 
    =  [\-]? SpecialInt [l]
    ;  
 
token Int64Literal 
    = [\-]? SpecialInt [L]
    ;  
 
token NativeIntLiteral 
    =  [\-]? SpecialInt [n]
    ;

token SpecialInt 
    = [0-9] [0-9_]* 
    | ("0x"| "0X") [0-9A-Fa-f][0-9A-Fa-f_]*   
    | ("0o"| "0O") [0-7] [0-7_]*
    | ("0b"| "0B") [0-1] [0-1_]*
    ;

token IntegerLiteral 
    = [\-]? [0-9] [0-9_]*
    | [\-]? ("0x"| "0X") [0-9A-Fa-f][0-9A-Fa-f_]*  
    | [\-]? ("0o"| "0O") [0-7] [0-7_]* 
    | [\-]? ("0b"| "0B") [0-1] [0-1_]* 
 	;
 					   
token FloatLiteral 
    = [\-]? [0-9] [0-9_]* [eE] [+\-]? [0-9] [0-9_]*              // only with e
	| [\-]? [0-9] [0-9_]* [.] [0-9_]*                      		 // only with .
    | [\-]? [0-9] [0-9_]* [.] [0-9_]* [eE] [+\-]? [0-9] [0-9_]*  // with both . and e
	;
					 
token CharLiteral 
    = [\'] (RegularChar | EscapeSequence) [\']
    ;
                            
token EscapeSequence 
    = ([\\] [\\ \" \' n t b r])
	| ([\\] [0-9][0-9][0-9])
	| ([\\][x] [0-9A-Fa-f][0-9A-Fa-f])
	;
                            
token StringLiteral1 
    = [\"] StringCharacter* [\"]
    ;

token StringCharacter 
    = RegularCharStr 
    | EscapeSequence 
    | [\\][\n] 
    | [\\][\ ]
    ;

token RegularChar 
    = ![\'\\]
    ;

token RegularCharStr 
    = ![\"\\]
    ;

token OperatorChar 
    = [! $ % & * + \- . / : \< = \> ? @ ^ | ~]
    ;

lexical PrefixSymbol 
      = ([!] OperatorChar*) \ "!="
   	  | [? ~] OperatorChar+ 
	  ;

lexical Label 
      =	"~" LowercaseIdentifier !>> ":"
      ;
                       
lexical LabelColon 
      = "~" LowercaseIdentifier ":"
      ;
      
lexical OptLabel 
      = "?" LowercaseIdentifier !>> ":"
      ;
      
lexical OptLabelColon 
      = "?" LowercaseIdentifier ":"
      ;	
      
lexical InfixSymbol
      = InfixSymbol1
      | InfixSymbol2
      | InfixSymbol3
      | InfixSymbol4
      | InfixSymbol5
      | InfixSymbol6
      | InfixSymbol7
      | InfixSymbol8   
      ;      
      
token InfixSymbol1 
      = "lsl" | "lsr" | "asr" 
      | [*][*] OperatorChar+
      ;
     
lexical InfixSymbol2 
      = "mod" | "land"| "lor" | "lxor" 
      | ([/ % *] OperatorChar*) \ "**"
      ;
       
lexical InfixSymbol3 
      = ([+ \-] OperatorChar*) \ "-\>"
      ;
      
token InfixSymbol4 
      = [@ ^] OperatorChar*
      ;
      
lexical InfixSymbol5 
      = "!="
      | ([ = \< \> | & $] OperatorChar*) \ InfixSymbol5Exclude 
      ;
      
token InfixSymbol5Exclude 
      = "|" 
      | "||" 
      | "&&" 
      | "&" 
      | "\<-"
      ;
      
token InfixSymbol6 
	  = "&" 
      | "&&"
      ;
                            
token InfixSymbol7 
      = "||" 
      | "or"
      ;
      
token InfixSymbol8 
      = ":="
      ;

keyword Keywords
      = "_"
      | "and"
	  | "as"
	  | "assert"
	  | "asr"
	  | "begin"
	  | "class"
	  | "constraint"
	  | "do"
	  | "done"
	  | "downto"
	  | "else"
	  | "end"
	  | "exception"
	  | "external"
	  | "false"
	  | "for"
	  | "fun"
	  | "function"
	  | "functor"
	  | "if"
      | "in"
	  | "include"
	  | "inherit"
	  | "initializer"
	  | "land"
	  | "lazy"
	  | "let"
	  | "lor"
	  | "lsl"
	  | "lsr"
	  | "lxor"
	  | "match"
	  | "method"
	  | "mod"
	  | "module"
	  | "mutable"
	  | "new"
	  | "object"
	  | "of"
	  | "open"
	  | "or"
	  | "private"
	  | "rec"
	  | "sig"
	  | "struct"
	  | "then"
	  | "to"
	  | "true"
	  | "try"
	  | "type"
	  | "val"
	  | "virtual"
	  | "when"
	  | "while"
	  | "with"
	  ;
	  

lexical Comment 
      = "(*" (![(*] | Comment | "*" !>> [)] | "(" !>> [*])* "*)"
      ;         
	
token Whitespace 
    = [\ \t\n\r \u0009-\u000D]+
    ;
	
layout Layout 
     = (Comment | Whitespace)* !>> [\ \t\n\r \u0009-\u000D] !>> "(*"
     ;
