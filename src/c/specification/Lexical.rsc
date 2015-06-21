

// Lexical elements

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

lexical keyword
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

