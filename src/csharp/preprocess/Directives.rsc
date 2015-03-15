module csharp::preprocess::Directives

// Pre-processing directives

lexical PpDirective
      = PpDeclaration
      | PpConditional
      | PpLine
      | PpDiagnostic
      | PpRegion 
      | PpPragma
      ;
      
lexical ConditionalSymbol
      = IdentifierOrKeyword \ "true" \ "false"
      ;
      
lexical PpExpression
      = Whitespace?   PpOrExpression   Whitespace?
      ;

lexical PpOrExpression
      = PpAndExpression
      | PpOrExpression   Whitespace?   "||"   Whitespace?   PpAndExpression
      ;

lexical PpAndExpression
      = PpEqualityExpression
      | PpAndExpression   Whitespace?   "&&"   Whitespace?   PpEqualityExpression
      ;

lexical PpEqualityExpression
      = PpUnaryExpression
      | PpEqualityExpression   Whitespace?   "=="   Whitespace?   PpUnaryExpression
      | PpEqualityExpression   Whitespace?   "!="   Whitespace?   PpUnaryExpression
      ;

lexical PpUnaryExpression
      = PpPrimaryExpression
      | "!"   Whitespace?   PpUnaryExpression
      ;
      
lexical PpPrimaryExpression
     = "true"
     | "false"
     ;
     
lexical ConditionalSymbol
     = "("   Whitespace?   PpExpression   Whitespace?   ")"
     ;

lexical PpDeclaration
      = Whitespace?   "#"   Whitespace?   ("define" | "undef")   Whitespace   ConditionalSymbol   PpNewLine
      ;
      
lexical PpNewLine 
      = Whitespace?   SingleLineComment?   NewLine
      ;

lexical PpConditional 
      = PpIfSection   PpElifSection*   PpElseSection?   PpEndif
      ;

lexical PpIfSection 
      = Whitespace?   "#"   Whitespace?   "if"   Whitespace   PpExpression   PpNewLine   ConditionalSection?
      ;

lexical PpElifSection
      = Whitespace?   "#"   Whitespace?   "elif"   Whitespace   PpExpression   PpNewLine   ConditionalSection?
      ;

lexical PpElseSection
      = Whitespace?   "#"   Whitespace?   "else"   PpNewLine   ConditionalSection?
      ;

lexical PpEndif
      = Whitespace?   "#"   Whitespace?   "endif"   PpNewLine
      ;

lexical ConditionalSection 
     = InputSection
     | SkippedSectionPart+
     ;

lexical SkippedSectionPart 
      = SkippedCharacters?   NewLine
      | PpDirective
      ;

lexical SkippedCharacters
     = Whitespace?   ![#]   InputCharacter*
     ;

lexical PpDiagnostic
     = Whitespace?   "#"   Whitespace?   ("error" | "warning")   PpMessage
     ;

lexical PpMessage
      = NewLine
      | Whitespace   InputCharacter*   NewLine
      ;

lexical PpRegion 
      = PpStartRegion   ConditionalSection?   PpEndRegion
      ;

lexical PpStartRegion 
      = Whitespace?   "#"   Whitespace?   "region"   PpMessage
      ;

lexical PpEndRegion 
      = Whitespace?   "#"   Whitespace?   "endregion"   PpMessage
      ;
      
      
lexical PpLine
     =  Whitespace?   "#"   Whitespace?   "line"   Whitespace   LineIndicator   PpNewLine
     ;
     
lexical LineIndicator 
     = DecimalDigit+   Whitespace   FileName 
     | DecimalDigit+
     | "default" 
     | "hidden"
     ;

lexical FileName 
     = "\""   FileNameCharacter+   "\""
     ;

lexical FileNameCharacter 
     = ![] \ [\"]
     ;

lexical PpPragma 
     = // Whitespace?   "#"   Whitespace?   "pragma"   Whitespace   PragmaBody   PpNewLine
     "#"   Whitespace?   "pragma"   Whitespace   PragmaBody
     ;

lexical PragmaBody 
     = PragmaWarningBody
     ;

lexical PragmaWarningBody 
     = "warning"   Whitespace   WarningAction   (Whitespace   WarningList)?
     ;

lexical WarningAction 
      = "disable"
      | "restore"
      ;

lexical WarningList 
      = DecimalDigit+
      | WarningList   Whitespace?   ","   Whitespace?   DecimalDigit+
      ;
