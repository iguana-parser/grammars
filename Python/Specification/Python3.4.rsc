/**
 *
 *  Python 3.4 
 *
 **/
 
module Python

syntax SingleInput 
     = NewLine 
     | SimpleStmt 
     | CompoundStmt NewLine
     ;

syntax FileInput 
     =  (NewLine | Stmt)* EndMarker
     ;

syntax EvalInput 
     =  TestList NewLine* EndMarker
     ;

syntax Decorator 
     =  "@" DottedName ( "(" Arglist? ")" )? NewLine
     ;

syntax Decorators 
     =  Decorator+
     ;

syntax Decorated 
     =  Decorators (Classdef | Funcdef)
     ;

syntax Funcdef 
     =  "def" Name Parameters ("-\>" Test)? ":" Suite
     ;

syntax Parameters 
     =  "(" [Typedargslist] ")"
     ;

syntax Typedargslist 
     = Tfpdef ("=" Test)? ("," Tfpdef ("=" Test)?)* ("," ( ("*" Tfpdef? ("," Tfpdef ("=" Test)?)* ("," "**" Tfpdef)?) 
     						                                         | ("**" Tfpdef)
                                                         )? 
                                                    )?
     | "*" Tfpdef? ("," Tfpdef ("=" Test)?)* ("," "**" Tfpdef)?
     | "**" Tfpdef
     ;

syntax Tfpdef 
     =  Name (":" Test)?
     ;

syntax Varargslist 
     = Vfpdef ("=" Test)? ("," Vfpdef ("=" Test)?)* ("," ( ("*" Vfpdef? ("," Vfpdef ("=" Test)?)* ("," "**" Vfpdef)?) 
                                                         | ("**" Vfpdef)
                                                         )? 
                                                    )?
     | "*" Vfpdef? ("," Vfpdef ("=" Test)?)* ("," "**" Vfpdef)?
     | "**" Vfpdef
     ;

syntax Vfpdef 
     = Name
     ;

syntax Stmt 
     = SimpleStmt 
     | CompoundStmt
     ;

syntax SimpleStmt 
    =  SmallStmt (";" SmallStmt)* ";"? NewLine
    ;

syntax SmallStmt 
     = ExprStmt 
     | DelStmt 
     | PassStmt 
     | FlowStmt 
     | ImportStmt 
     | GlobalStmt 
     | NonlocalStmt 
     | AssertStmt
     ;

syntax ExprStmt 
     =  TestlistStarExpr ( (Augassign (YieldExpr | TestList)) 
                         | ("=" (YieldExpr | TestlistStarExpr) )* 
                         )
     ;

syntax TestlistStarExpr 
     =  (Test | StarExpr) ("," (Test | StarExpr))* ","?
     ;

syntax Augassign 
     = "+=" 
     | "-=" 
     | "*=" 
     | "/=" 
     | "%=" 
     | "&=" 
     | "|=" 
     | "^=" 
     | "\<\<=" 
     | "\>\>=" 
     | "**=" 
     | "//="
     ;

// For normal assignments, additional restrictions enforced by the interpreter

syntax DelStmt 
     =  "del" Exprlist
     ;

syntax PassStmt 
     =  "pass"
     ;

syntax FlowStmt 
     = BreakStmt 
     | ContinueStmt 
     | ReturnStmt 
     | RaiseStmt 
     | YieldStmt
     ;

syntax BreakStmt 
     =  "break"
     ;

syntax ContinueStmt 
     =  "continue"
     ;

syntax ReturnStmt 
     =  "return" TestList?
     ;

syntax YieldStmt 
     =  YieldExpr
     ;

syntax RaiseStmt 
     =  "raise" (Test ("from" Test)? )?
     ;

syntax ImportStmt 
     =  ImportName 
     |  ImportFrom
     ;

syntax ImportName 
     =  "import" DottedAsNames
     ;

// note below =  the ("." | "...") is necessary because "..." is tokenized as ELLIPSIS

syntax ImportFrom 
     =  "from" ( (("." | "...")* DottedName)
               | ("." | "...")+
               ) 
        "import" ("*" 
                 | ("(" ImportAsNames ")") 
                 | ImportAsNames
                 )
     ;

syntax ImportAsName 
    =  Name ("as" Name)?
    ;

syntax DottedAsName 
     =  DottedName ("as" Name)?
     ;

syntax ImportAsNames 
     =  ImportAsName ("," ImportAsName)* ","?
     ;

syntax DottedAsNames 
     =  DottedAsName ("," DottedAsName)*
     ;

syntax DottedName 
     =  Name ("." Name)*
     ;

syntax GlobalStmt 
     =  "global" Name ("," Name)*
     ;

syntax NonlocalStmt 
     =  "nonlocal" Name ("," Name)*
     ;

syntax AssertStmt 
     =  "assert" Test ("," Test)?
     ;

syntax CompoundStmt 
     = IfStmt 
     | WhileStmt 
     | ForStmt 
     | TryStmt 
     | WithStmt 
     | Funcdef 
     | Classdef 
     | Decorated
     ;

syntax IfStmt 
     =  "if" Test ":" Suite ("elif" Test ":" Suite)* ("else" ":" Suite)?
     ;

syntax WhileStmt 
     =  "while" Test ":" Suite ("else" ":" Suite)?
     ;

syntax ForStmt 
     =  "for" Exprlist "in" TestList ":" Suite ("else" ":" Suite)?
     ;

syntax TryStmt 
     =  "try" ":" Suite ( ((ExceptClause ":" Suite)+ ("else" ":" Suite)? ("finally" ":" Suite)?)
                        | ("finally" ":" Suite)
                        )
     ;

syntax WithStmt 
     =  "with" WithItem ("," WithItem)*  ":" Suite
     ;

syntax WithItem 
     =  Test ("as" Expr)?
     ;

// NB compile.c makes sure that the default except clause is last

syntax ExceptClause 
     =  "except" (Test ("as" Name)?)?
     ;

syntax Suite 
     =  SimpleStmt 
     | NewLine INDENT Stmt+ DEDENT
     ;


syntax Test 
     =  OrTest ("if" OrTest "else" Test)?
     | Lambdef
     ;

syntax TestNocond 
     =  OrTest 
     | LambdefNocond
     ;

syntax Lambdef 
     =  "lambda" Varargslist? ":" Test
     ;

syntax LambdefNocond 
     =  "lambda" Varargslist? ":" TestNocond
     ;

syntax OrTest 
     =  AndTest ("or" AndTest)*
     ;

syntax AndTest 
     =  NotTest ("and" NotTest)*
     ;

syntax NotTest 
     =  "not" NotTest 
     | Comparison
     ;

syntax Comparison 
     =  Expr (CompOp Expr)*
     ;

// <> isn't actually a valid comparison operator in Python. It's here for the
// sake of a __future__ import described in PEP 401

syntax CompOp 
     = "\<"
     | "\>"
     | "=="
     | "\>="
     | "\<="
     | "\<\>"
     | "!="
     | "in"
     | "not" "in"
     | "is"
     | "is" "not"
     ;

syntax StarExpr 
     =  "*" Expr
     ;

syntax Expr 
     =  XorExpr ("|" XorExpr)*
     ;

syntax XorExpr 
     =  AndExpr ("^" AndExpr)*
     ;

syntax AndExpr 
    =  ShiftExpr ("&" ShiftExpr)*
    ;

syntax ShiftExpr 
     =  ArithExpr (( "\<\<" | "\>\>" ) ArithExpr)*
     ;

syntax ArithExpr 
     =  Term (("+"|"-") Term)*
     ;

syntax Term 
     =  Factor (("*"|"/"|"%"|"//") Factor)*
     ;

syntax Factor 
     =  ("+"|"-"|"~") Factor 
     | Power
     ;

syntax Power 
    =  Atom Trailer* ("**" Factor)?
    ;

syntax Atom 
     = "(" [YieldExpr|TestlistComp] ")" 
     | "[" [TestlistComp] "]" 
     | "{" [Dictorsetmaker] "}" 
     | Name 
     | Number 
     | STRING+ 
     | "..." 
     | "None" 
     | "True" 
     | "False"
     ;

syntax TestlistComp 
     =  (Test | StarExpr) ( CompFor | (("," (Test|StarExpr))* ","?) )
     ;

syntax Trailer 
     =  "(" Arglist? ")" 
     | "[" Subscriptlist "]" 
     | "." Name
     ;

syntax Subscriptlist 
     =  Subscript ("," Subscript)* ","?
     ;

syntax Subscript 
     = Test 
     | Test? ":" Test? Sliceop?
     ;

syntax Sliceop 
     =  ":" Test?
     ;

syntax Exprlist 
     =  (Expr | StarExpr) ("," (Expr | StarExpr))* ","?
     ;

syntax TestList 
     =  Test ("," Test)* ","?
     ;

syntax Dictorsetmaker 
     = Test ":" Test (CompFor | (("," Test ":" Test)* ","?))
     | Test (CompFor | (("," Test)* ","?))
     ;


syntax Classdef 
     =  "class" Name ("(" Arglist? ")")? ":" Suite
     ;


syntax Arglist 
     =  (Argument ",")* ( (Argument ","?) 
                        | ("*" Test ("," Argument)* ("," "**" Test)?)
                        | ("**" Test)
                        )
     ;

// The reason that keywords are test nodes instead of Name is that using Name
// results in an ambiguity. ast.c makes sure it's a Name.

syntax Argument 
     =  Test CompFor?
     | Test "=" Test  // Really [keyword "="] test
     ;

syntax CompIter 
     =  CompFor 
     | CompIf
     ;

syntax CompFor 
     =  "for" Exprlist "in" OrTest CompIter?
     ;

syntax CompIf 
    =  "if" TestNocond CompIter?
    ;

// not used in grammar, but may appear in "node" passed from Parser to Compiler

syntax EncodingDecl 
     =  Name
     ;

syntax YieldExpr 
     =  "yield" YieldArg?
     ;

syntax YieldArg 
     =  "from" Test 
     | TestList
     ;
     
     
// Lexicals

keyword Keyword
  	  = "False"
      | "class"
      | "finally"
      | "is"
      | "return"
      | "None"
      | "continue"
      | "for"
      | "lambda"
      | "try"
      | "True"
      | "def"
      | "from"
      | "nonlocal"
      | "while"
      | "and"
      | "del"
      | "global"
      | "not"
      | "with"
      | "as"
      | "elif"
      | "if"
      | "or"
      | "yield"
      | "assert"
      | "else"
      | "import"
      | "pass"
      | "break"
      | "except"
      | "in"
      | "raise"
      ;

// Numbers

lexical Number
      = Integer
      | FloatNumber
      | ImagNumber
      ;

lexical Integer        
      = DecimalInteger 
      | OctInteger 
      | HexInteger 
      | BinInteger
      ;

lexical DecimalInteger 
      =  NonzeroDigit Digit* 
      | "0"+
      ;

lexical NonzeroDigit   
      =  [1-9]
      ;

lexical Digit          
      =  [0-9]
      ;

lexical OctInteger     
      =  "0" ("o" | "O") OctDigit+
      ;

lexical HexInteger     
      =  "0" ("x" | "X") HexDigit+
      ;

lexical BinInteger     
      =  "0" ("b" | "B") BinDigit+
      ;

lexical OctDigit       
      =  [0-7]
      ;

lexical HexDigit       
      =  Digit 
      | [a-f] 
      | [A-F]
      ;

lexical BinDigit       
      =  "0" 
      | "1"
      ;

lexical FloatNumber   
    = PointFloat 
    | ExponentFloat
    ;

lexical Pointfloat    
      = Intpart? Fraction 
      | Intpart "."
      ;

lexical Exponentfloat 
      =  (Intpart | Pointfloat) Exponent
      ;

lexical Intpart       
      =  Digit+
      ;

lexical Fraction      
      =  "." Digit+
      ;

lexical Exponent      
      =  ("e" | "E") ("+" | "-")? Digit+
      ;

lexical ImagNumber 
      =  (Floatnumber | Intpart) ("j" | "J")
      ;      
      
// String

lexical StringLiteral   
      = StringPrefix? (ShortString | LongString)
      ;

lexical StringPrefix
      = "R" | "u" | "R" | "U"
      ;

lexical ShortString
      = "\'" ShortStringItem* "\'" 
      | "\"" ShortStringItem* "\""
      ;

lexical LongString      
      = "\'\'\'" LongStringItem* "\'\'\'" 
      | "\"\"\"" LongStringItem* "\"\"\""
      ;

lexical ShortStringItem 
      = ShortStringChar 
      | StringEscapeSeq
      ;

lexical LongStringItem  
      = LongStringChar 
      | StringEscapeSeq
      ;

lexical ShortStringChar 
	     =  ![] \ [\" \\ \n]
	     ;

lexical LongStringChar  
     =  ![] \ [\\]
     ;

lexical StringEscapeSeq 
      = [\\][n]
      | [\\][\\]       
      | [\\][\']       
      | [\\][\"]       
      | [\\][a]       
      | [\\][b]
      | [\\][f]       
      | [\\][n]
      | [\\][r]       
      | [\\][t]       
      | [\\][v]       
      | [\\] OctInteger OctInteger OctInteger
      | [\\] [x] HexInteger HexInteger
      ;
      