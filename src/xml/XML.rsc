/**
 *  Ali Afroozeh
 */

module xml::XML

syntax Element      
     = EmptyElemTag
     | STag Content ETag
     ;

syntax EmptyElemTag 
     = "\<" Name (S Attribute)* S? "/\>"
     ;

syntax STag         
     = "\<" Name (S Attribute)* S? "\>"
     ;

syntax ETag         
     = "\</" Name S? "\>"
     ;

syntax Content      
     = CharData? (Content1 CharData?)*
     ;

syntax Content1     
     = XmlContent
     | Reference
     | ScalaExpr
     ;

syntax XmlContent   
     = Element
     | CDSect
     | PI
     | XmlComment
     ;

syntax Attribute 
     = Name Eq AttValue
     ;

syntax AttValue     
     = "\"" (CharQ | CharRef)* "\""
     | "\'" (CharA | CharRef)* "\'"
     | ScalaExpr
     ;

syntax ScalaExpr    
     = Block
     ;

syntax CharData     
     = Char1*
     ;

syntax Char1        
     =  Char  \ "\<" 
     | "&"
     ;

syntax CharQ        
     =  Char1  \ "\""
     ;

syntax CharA        
     =  Char1  \ "\'"
     ;

syntax CharB        
     =  Char1  \ "{"
     ;

syntax Name         
     =  XNameStart NameChar*
     ;

syntax XNameStart   
     = "_" 
     | BaseChar 
     | Ideographic \ [:]
     ;


syntax XmlPattern
     = ElemPattern
     ;

syntax ElemPattern 
     = EmptyElemTagP
     | STagP ContentP ETagP
     ;

syntax EmptyElemTagP
     = "\<"  Name [S] "/\>"
     ;

syntax STagP       
     = "\<"  Name [S] "\>"
     ;

syntax ETagP       
     = "\</" Name [S] "\>"
     ;

syntax ContentP    
     = CharData? ( (ElemPattern | ScalaPatterns) CharData? )*
     ;

syntax ContentP1   
     = ElemPattern
     | Reference
     | CDSect
     | PI
     | Comment
     | ScalaPatterns
     ;

syntax ScalaPatterns
     = "{" Patterns "}"
     ;
