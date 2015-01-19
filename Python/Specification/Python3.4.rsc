syntax single_input =  NEWLINE | simple_stmt | compound_stmt NEWLINE
syntax file_input =  (NEWLINE | stmt)* ENDMARKER
syntax eval_input =  testlist NEWLINE* ENDMARKER

syntax decorator =  '@' dotted_name [ '(' [arglist] ')' ] NEWLINE
syntax decorators =  decorator+
syntax decorated =  decorators (classdef | funcdef)
syntax funcdef =  'def' NAME parameters ['->' test] ':' suite
syntax parameters =  '(' [typedargslist] ')'
syntax typedargslist =  (tfpdef ['=' test] (',' tfpdef ['=' test])* [','
       ['*' [tfpdef] (',' tfpdef ['=' test])* [',' '**' tfpdef] | '**' tfpdef]]
     |  '*' [tfpdef] (',' tfpdef ['=' test])* [',' '**' tfpdef] | '**' tfpdef)
syntax tfpdef =  NAME [':' test]
syntax varargslist =  (vfpdef ['=' test] (',' vfpdef ['=' test])* [','
       ['*' [vfpdef] (',' vfpdef ['=' test])* [',' '**' vfpdef] | '**' vfpdef]]
     |  '*' [vfpdef] (',' vfpdef ['=' test])* [',' '**' vfpdef] | '**' vfpdef)
syntax vfpdef =  NAME

syntax stmt =  simple_stmt | compound_stmt
syntax simple_stmt =  small_stmt (';' small_stmt)* [';'] NEWLINE
syntax small_stmt =  (expr_stmt | del_stmt | pass_stmt | flow_stmt |
             import_stmt | global_stmt | nonlocal_stmt | assert_stmt)
syntax expr_stmt =  testlist_star_expr (augassign (yield_expr|testlist) |
                     ('=' (yield_expr|testlist_star_expr))*)
syntax testlist_star_expr =  (test|star_expr) (',' (test|star_expr))* [',']
syntax augassign =  ('+=' | '-=' | '*=' | '/=' | '%=' | '&=' | '|=' | '^=' |
            '<<=' | '>>=' | '**=' | '//=')
# For normal assignments, additional restrictions enforced by the interpreter
syntax del_stmt =  'del' exprlist
syntax pass_stmt =  'pass'
syntax flow_stmt =  break_stmt | continue_stmt | return_stmt | raise_stmt | yield_stmt
syntax break_stmt =  'break'
syntax continue_stmt =  'continue'
syntax return_stmt =  'return' [testlist]
syntax yield_stmt =  yield_expr
syntax raise_stmt =  'raise' [test ['from' test]]
syntax import_stmt =  import_name | import_from
syntax import_name =  'import' dotted_as_names
syntax # note below =  the ('.' | '...') is necessary because '...' is tokenized as ELLIPSIS
syntax import_from =  ('from' (('.' | '...')* dotted_name | ('.' | '...')+)
              'import' ('*' | '(' import_as_names ')' | import_as_names))
syntax import_as_name =  NAME ['as' NAME]
syntax dotted_as_name =  dotted_name ['as' NAME]
syntax import_as_names =  import_as_name (',' import_as_name)* [',']
syntax dotted_as_names =  dotted_as_name (',' dotted_as_name)*
syntax dotted_name =  NAME ('.' NAME)*
syntax global_stmt =  'global' NAME (',' NAME)*
syntax nonlocal_stmt =  'nonlocal' NAME (',' NAME)*
syntax assert_stmt =  'assert' test [',' test]

syntax compound_stmt =  if_stmt | while_stmt | for_stmt | try_stmt | with_stmt | funcdef | classdef | decorated
syntax if_stmt =  'if' test ':' suite ('elif' test ':' suite)* ['else' ':' suite]
syntax while_stmt =  'while' test ':' suite ['else' ':' suite]
syntax for_stmt =  'for' exprlist 'in' testlist ':' suite ['else' ':' suite]
syntax try_stmt =  ('try' ':' suite
           ((except_clause ':' suite)+
            ['else' ':' suite]
            ['finally' ':' suite] |
           'finally' ':' suite))
syntax with_stmt =  'with' with_item (',' with_item)*  ':' suite
syntax with_item =  test ['as' expr]
# NB compile.c makes sure that the default except clause is last
syntax except_clause =  'except' [test ['as' NAME]]
syntax suite =  simple_stmt | NEWLINE INDENT stmt+ DEDENT

syntax test =  or_test ['if' or_test 'else' test] | lambdef
syntax test_nocond =  or_test | lambdef_nocond
syntax lambdef =  'lambda' [varargslist] ':' test
syntax lambdef_nocond =  'lambda' [varargslist] ':' test_nocond
syntax or_test =  and_test ('or' and_test)*
syntax and_test =  not_test ('and' not_test)*
syntax not_test =  'not' not_test | comparison
syntax comparison =  expr (comp_op expr)*
# <> isn't actually a valid comparison operator in Python. It's here for the
# sake of a __future__ import described in PEP 401
syntax comp_op =  '<'|'>'|'=='|'>='|'<='|'<>'|'!='|'in'|'not' 'in'|'is'|'is' 'not'
syntax star_expr =  '*' expr
syntax expr =  xor_expr ('|' xor_expr)*
syntax xor_expr =  and_expr ('^' and_expr)*
syntax and_expr =  shift_expr ('&' shift_expr)*
syntax shift_expr =  arith_expr (('<<'|'>>') arith_expr)*
syntax arith_expr =  term (('+'|'-') term)*
syntax term =  factor (('*'|'/'|'%'|'//') factor)*
syntax factor =  ('+'|'-'|'~') factor | power
syntax power =  atom trailer* ['**' factor]
syntax atom =  ('(' [yield_expr|testlist_comp] ')' |
       '[' [testlist_comp] ']' |
       '{' [dictorsetmaker] '}' |
       NAME | NUMBER | STRING+ | '...' | 'None' | 'True' | 'False')
syntax testlist_comp =  (test|star_expr) ( comp_for | (',' (test|star_expr))* [','] )
syntax trailer =  '(' [arglist] ')' | '[' subscriptlist ']' | '.' NAME
syntax subscriptlist =  subscript (',' subscript)* [',']
syntax subscript =  test | [test] ':' [test] [sliceop]
syntax sliceop =  ':' [test]
syntax exprlist =  (expr|star_expr) (',' (expr|star_expr))* [',']
syntax testlist =  test (',' test)* [',']
syntax dictorsetmaker =  ( (test ':' test (comp_for | (',' test ':' test)* [','])) |
                  (test (comp_for | (',' test)* [','])) )

syntax classdef =  'class' NAME ['(' [arglist] ')'] ':' suite

syntax arglist =  (argument ',')* (argument [',']
                         |'*' test (',' argument)* [',' '**' test] 
                         |'**' test)
# The reason that keywords are test nodes instead of NAME is that using NAME
# results in an ambiguity. ast.c makes sure it's a NAME.
syntax argument =  test [comp_for] | test '=' test  # Really [keyword '='] test
syntax comp_iter =  comp_for | comp_if
syntax comp_for =  'for' exprlist 'in' or_test [comp_iter]
syntax comp_if =  'if' test_nocond [comp_iter]

# not used in grammar, but may appear in "node" passed from Parser to Compiler
syntax encoding_decl =  NAME

syntax yield_expr =  'yield' [yield_arg]
syntax yield_arg =  'from' test | testlist