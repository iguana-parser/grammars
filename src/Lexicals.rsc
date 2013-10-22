module Lexicals

lexical HexaFloatLiteral = HexaFloatNumeral [D F d f]?;

lexical HexaFloatNumeral = HexaSignificand !>> [0-9 A-F a-f] BinaryExponent;
  //HexaSignificand \ HexaSignificandKeywords !>> [0-9 A-F a-f] BinaryExponent 

lexical OctaLiteral = OctaNumeral !>> [0-7] [L l]?;

lexical Comment 
		= "/*" (![*]|[\r\n]|([*]+(![*/]|[\r\n])))* [*]+ "/"
		| "//" ![\n]* !>> [\ \t\r \u00A0 \u1680 \u2000-\u200A \u202F \u205F \u3000] $
		; 

lexical CharContent = EscapeSeq 
  					| UnicodeEscape 
  					|  single: SingleChar 
  					;

lexical StringChars = FooStringChars;

lexical DeciNumeral = [1-9] [0-9]* | "0";

lexical OctaEscape 
  = "\\" [0-3] [0-7]+ !>> [0-7] 
  | "\\" [0-7] !>> [0-7] 
  | "\\" [4-7] [0-7] 
  ;

lexical EscChar = "\\";

lexical LEX[CharLiteral] = char: "\'" CharContent "\'";

lexical HexaSignificand =
  [0] [X x] [0-9 A-F a-f]* "." [0-9 A-F a-f]* 
  | [0] [X x] [0-9 A-F a-f]+ 
  ;

lexical OctaNumeral =
  [0] [0-7]+ 
  ;

lexical HexaNumeral =
  [0] [X x] [0-9 A-F a-f]+ 
  ;
  
lexical LEX[StringLiteral] =
   string: "\"" StringPart* "\"" 
  ;

lexical SignedInteger = [+ \-]? [0-9]+;

lexical HexaLiteral = HexaNumeral !>> [0-9 A-F a-f] [L l]?;

lexical DeciFloatLiteral = DeciFloatNumeral [D F d f]?;

lexical ID = [$ A-Z _ a-z] [$ 0-9 A-Z _ a-z]*;

lexical DeciFloatDigits = [0-9]+ 
						| [0-9]* "." [0-9]* 
  						;

lexical DeciLiteral = DeciNumeral !>> [. 0-9 D F d f] [L l]?;

lexical EscapeSeq 
	 =  NamedEscape 
  	 | OctaEscape 
  	 ;
  
lexical StringPart =
  UnicodeEscape 
  | EscapeSeq 
  |  chars: StringChars !>> ![\n \a0D \" \\]  !>> [\a00]
  ;

lexical EOLCommentChars =
  ![\n \a0D]* 
  ;
  
lexical SingleChar =
  ![\n \a0D \' \\] 
  ;

lexical DeciFloatExponentPart =
  [E e] SignedInteger !>> [0-9] 
  ;
  
lexical DeciFloatNumeral
	= [0-9] !<< [0-9]+ DeciFloatExponentPart
	| [0-9] !<< [0-9]+ >> [D F d f]
	| [0-9] !<< [0-9]+ "." [0-9]* !>> [0-9] DeciFloatExponentPart?
	| [0-9] !<< "." [0-9]+ !>> [0-9] DeciFloatExponentPart?
  ;

lexical UnicodeEscape =
   unicodeEscape: "\\" [u]+ [0-9 A-F a-f] [0-9 A-F a-f] [0-9 A-F a-f] [0-9 A-F a-f] 
  ;
  	
  	
lexical NamedEscape =
   namedEscape: "\\" [\" \' \\ b f n r t] 
  ;
  
lexical BinaryExponent =
  [P p] SignedInteger !>> [0-9] 
  ;

lexical BlockCommentChars =
  ![* \\]+ 
  ;

lexical FooStringChars =
  ([\a00] | ![\n \a0D \" \\])+ 
  ;
    	
  					
layout Layout = LayoutElement* !>> [\ \t\n\r \u0009-\u000D] !>> "/*" !>> "//";

lexical LayoutElement = Comment | Whitespace;

lexical Whitespace =  [\ \t\n\r \u0009-\u000D]+;
  					


