! Copyright (C) 2009 Tim Wawrzynczak
! See http://factorcode.org/license.txt for BSD license.
USING: kernel arrays strings joy.ast make combinators
accessors sequences math.parser prettyprint ;
IN: joy.pprint

! pretty-printing of a joy AST

GENERIC: (@pprint) ( ast -- )

M: object (@pprint) ( obj -- )
     unparse , ;

M: ast-quotation (@pprint) ( ast -- )
    CHAR: [ , " " % body>> [ (@pprint) " " % ] each CHAR: ] , ;

M: ast-string (@pprint) ( ast -- )
    CHAR: " , string>> % CHAR: " , ;

M: ast-character (@pprint) ( ast -- )
    CHAR: ' , char>> , ;

M: ast-number (@pprint) ( ast -- )
    num>> # ;

M: ast-identifier (@pprint) ( ast -- )
    name>> % ;

M: ast-special (@pprint) ( ast -- )
    value>> % ;

M: ast-definition (@pprint) ( ast -- )
    "DEFINE " , dup name>> , " == " ,
    body>> [ , " " , ] each ;

M: ast-module-keyword (@pprint) ( ast -- )
    value>> % ;

M: ast-boolean (@pprint) ( ast -- )
    value>> % ;

: @pprint ( ast -- string )
  [ (@pprint) ] "" make ;
