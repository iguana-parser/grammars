
module c::specification::Preprocessor

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

lexical EndIfLine
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
      = PreProcessingToken
      | PpTokens PreprocessingToken
      ;

lexical NewLIne 
      = [\r \n]
      ;
