! Copyright (C) 2009 Tim Wawrzynczak
! See http://factorcode.org/license.txt for BSD license.
USING: kernel namespaces sequences accessors make joy.ast 
combinators prettyprint math math.ranges random calendar 
math.functions math.order strings io io.files destructors 
arrays io.directories continuations io.encodings.utf8 
macros stack-checker fry generalizations quotations math.parser 
locals sequences.deep compiler.units words vocabs.parser
assocs lists joy.cons ;
IN: joy.compiler

GENERIC: (compile) ( ast -- )

! printing
: (put) ( o -- )
    {
        { [ dup string? ] [ print ] }
        { [ dup number? ] [ number>string print ] }
    } cond ; inline

! time
: (time) ( -- time )
    now
    1970 1 1 0 0 0 0 hours <timestamp>
    time- duration>seconds floor ; inline

! number-related words

: (sign) ( n -- n' )
    {
        { [ dup 0 > ] [ drop 1 ] }
        { [ dup 0 < ] [ drop -1 ] }
        [ drop 0 ]
    } cond ; inline

: (neg) ( n -- -n ) 0 swap - ; inline

: (log10) ( n -- n' )
    log 10 log / ; inline

! sequence words

: (rotate) ( x y z -- z y x ) swapd swap swapd ; inline

: (enconcat) ( x s t -- u )
    swapd swap prefix append ; inline

: (null) ( l/n -- ? )
    {
        { [ dup sequence? ] [ empty? ] }
        { [ dup number? ] [ 0 = ] }
        [ ": Invalid operands" append throw ]
    } cond ; inline

: (small) ( l/n -- ? )
    {
        { [ dup sequence? ] [ length [ 0 = ] [ 1 = ] bi or ] }
        { [ dup number? ] [ [ 0 = ] [ 1 = ] bi or ] }
        [ ": Invalid operands" append throw ]
    } cond ; inline

! file words

: (fopen) ( path mode -- stream )
    [ dup string? [ "Not a filename!" throw ] unless ] dip 
    {
        { [ "r" over start ] [ drop utf8 <file-reader> ] }
        { [ "w" over start ] [ drop utf8 <file-writer> ] }
        { [ "a" over start ] [ drop utf8 <file-appender> ] }
        [ drop "You must specify a valid file mode!" throw ]
    } cond ; inline
: (fflush) ( stream -- ) stream-flush ; inline

: (fgetch) ( stream -- stream elt )
    dup stream-read1 1array >string ; inline

: (fgets) ( stream -- stream line )
    dup stream-readln ; inline

: (fread) ( stream i -- stream lst )
    dupd stream-read ; inline

: (fwrite) ( stream list -- stream )
    dupd stream-write ; inline

: (fremove) ( pathname -- ? )
    [ delete-file t ] [ 2drop f ] recover ; inline

: (frename) ( p1 p2 -- ? )
    [ move-file t ] [ 3drop f ] recover ; inline

: (fputch) ( stream char -- stream )
    dupd stream-write1 ; inline

: (fputchars) ( stream list -- stream ) (fwrite) ; inline

: (fclose) ( stream -- ) dispose ; inline

! ***********
! combinators
! ***********

! macro/ifte borrowed from Slava
MACRO: preserving ( quot -- )
    [ infer in>> length ] keep '[ _ ndup @ ] ;
: (ifte) ( pred t f -- )
    [ preserving ] 2dip if ; inline

: (while) ( bquot quot -- )
    swap [ keep ] curry [ swap ] compose
    do compose [ loop ] curry when ; inline

: (step) ( seq quot -- ) each ; inline

: (fold) ( list init quot -- ) reduce ; inline

:: (linrec) ( if-quot then-quot else1-quot else2-quot -- )
    if-quot preserving
    [ then-quot call ]
    [ else1-quot call
      if-quot then-quot else1-quot else2-quot (linrec)
      else2-quot call
    ] if ; inline recursive

:: (tailrec) ( if-quot then-quot else-quot -- )
    if-quot preserving
    [ then-quot call ]
    [ else-quot call
      if-quot then-quot else-quot (tailrec)
    ] if ; inline recursive      

:: (binrec) ( if-quot then-quot prod-quot comb-quot -- )
    if-quot preserving
    [ then-quot call ]
    [ prod-quot call
      if-quot then-quot prod-quot comb-quot (binrec)
      [ if-quot then-quot prod-quot comb-quot (binrec) ] dip
      if-quot then-quot prod-quot comb-quot (binrec)
      comb-quot call
    ] if ; inline recursive

! compiling
: default-env ( -- env )
    H{
        { "dup"       [ \ dup , ] }
        { "dip"       [ \ dip , ] }
        { "swap"      [ \ swap , ] }
        { "rollup"    [ \ -rot , ] }
        { "rolldown"  [ \ rot , ] }
        { "rotate"    [ \ (rotate) , ] }
        { "pop"       [ \ drop , ] }
        { "dupd"      [ \ dupd , ] }
        { "swapd"     [ \ swapd , ] }
        { "rollupd"   [ [ -rot ] , \ dip , ] }
        { "rolldownd" [ [ rot ] , \ dip , ] }
        { "rotated"   [ [ (rotate) ] , \ dip , ] }
        { "popd"      [ [ drop ] , \ dip , ] }
        
        { "put"      [ \ (put) , ] }
        { "putchars" [ \ print , ] }
        { "print"    [ \ pprint , ] }
        { "."        [ \ pprint , ] }
        { "putch"    [ \ write , ] }
        
        { "or"  [ \ or , ] }
        { "and" [ \ and , ] }
        { "xor" [ \ xor , ] }
        { "not" [ \ not , ] }
        
        { "+"     [ \ + , ] }
        { "*"     [ \ * , ] }
        { "/"     [ \ / , ] }
        { "-"     [ \ - , ] }
        { "rem"   [ \ mod , ] }
        { "div"   [ \ /mod , ] }
        { "ceil"  [ \ ceiling , ] }
        { "floor" [ \ floor , ] }
        { "exp"   [ \ exp , ] }
        { "trunc" [ \ truncate , ] }
        { "pred"  [ [ 1 - ] % ] }
        { "succ"  [ [ 1 + ] % ] }
        { "max"   [ \ max , ] }
        { "min"   [ \ min , ] }
        
        { "false" [ \ f , ] }
        { "true"  [ \ t , ] }
        { "rand"  [ 1 32767 [a,b] random , ] }
        { "id"    [ ] }
        { "time"  [ \ (time) , ] }
        { "sign"  [ \ (sign) , ] }
        { "neg"   [ \ (neg) , ] }
        { "abs"   [ \ abs , ] }
        
        { "cos"   [ \ cos , ] }
        { "sin"   [ \ sin , ] }
        { "tan"   [ \ tan , ] }
        { "acos"  [ \ acos , ] }
        { "asin"  [ \ asin , ] }
        { "atan"  [ \ atan , ] }
        { "cosh"  [ \ cosh , ] }
        { "sinh"  [ \ sinh , ] }
        { "tanh"  [ \ tanh , ] }
        { "log"   [ \ log , ] }
        { "log10" [ \ (log10) , ] }
        { "pow"   [ \ ^ , ] }
        { "sqrt"  [ \ sqrt , ] }
        
        { "cons"     [ [ swap prefix ] % ] }
        { "swons"    [ \ prefix , ] }
        { "first"    [ \ first , ] }
        { "rest"     [ \ rest , ] }
        { "of"       [ \ nth , ] }
        { "at"       [ [ swap nth ] % ] }
        { "size"     [ \ length , ] }
        { "uncons"   [ [ dup rest [ first ] dip ] % ] }
        { "drop"     [ \ tail , ] }
        { "take"     [ \ head , ] }
        { "concat"   [ \ append , ] }
        { "enconcat" [ \ (enconcat) , ] }
        { "swoncat"  [ [ swap append ] % ] }
        { "null"     [ \ (null) , ] }
        { "small"    [ \ (small) , ] }
        { "has"      [ [ swap member? ] % ] }
        { "in"       [ \ member? , ] }
        
        { ">=" [ \ >= , ] }
        { ">"  [ \ > , ] }
        { "<=" [ \ <= , ] }
        { "<"  [ \ < , ] }
        { "!=" [ [ = not ] % ] }
        { "="  [ \ = , ] }
        
        { "integer" [ \ integer? , ] }
        { "char"    [ [ [ string? ] [ length 1 = ] bi and ] % ] }
        { "logical" [ \ boolean? , ] }
        { "string"  [ \ string? , ] }
        { "list"    [ \ sequence? , ] }
        { "leaf"    [ [ sequence? not ] % ] }
        { "float"   [ \ float? , ] }
        
        { "fopen"     [ \ (fopen) , ] }
        { "fflush"    [ \ stream-flush , ] }
        { "fgetch"    [ \ (fgetch) , ] }
        { "fgets"     [ \ (fgets) , ] }
        { "fread"     [ \ (fread) , ] }
        { "fwrite"    [ \ (fwrite) , ] }
        { "fremove"   [ \ (fremove) , ] }
        { "frename"   [ \ (frename) , ] }
        { "fputch"    [ \ (fputch) , ] }
        { "fputchars" [ \ (fputchars) , ] }
        { "fclose"    [ \ (fclose) , ] }
        
        { "i"       [ \ call , ] }
        { "x"       [ [ dup call ] % ] }
        { "ifte"    [ \ (ifte) , ] }
        { "while"   [ \ (while) , ] }
        { "step"    [ \ (step) , ] }
        { "map"     [ \ map , ] }
        { "fold"    [ \ reduce , ] }
        { "times"   [ \ times , ] }
        { "linrec"  [ \ (linrec) , ] }
        { "tailrec" [ \ (tailrec) , ] }
        { "binrec"  [ \ (binrec) , ] }
        { "case"    [ \ case , ] }
    } ;

TUPLE: joy-env user-env env ;

SYMBOL: joy

: compile-identifier ( ident -- )
    dup joy get env>> at
    [ call( -- ) drop ]
    [ joy get user-env>> at
      [ call( -- ) , ]
      [ ": Unknown word!" append throw ] if*
    ] if* ; inline

: add-to-user-env ( word name -- )
    [ literalize 1quotation ] dip
    joy get user-env>> set-at ; inline

: add-word ( ast -- word )
    name>> current-vocab name>> create ; inline

: compile-definition ( word ast -- )
    body>> [ [ (compile) ] each ] [ ] make
    dup [ infer ] [ 2drop (( -- )) ] recover
    [ define-inline ] 3curry with-compilation-unit ; inline

: create-definition ( ast -- word )
    [ add-word ] [ name>> ] bi dupd ! word word name
    add-to-user-env ; inline

! generic eval word

M: joy-cons (compile) ( ast -- ) , ; inline

M: string (compile) ( ast -- ) drop ; inline

M: ast-definitions (compile) ( ast -- )
    definitions>> [ ast-definition? ] deep-filter [ (compile) ] each ; inline

M: ast-definition (compile) ( ast -- )
    dup create-definition swap
    compile-definition ; inline
    
M: ast-string (compile) ( ast -- ) string>> , ; inline

M: ast-number (compile) ( ast -- ) num>> , ; inline

M: ast-character (compile) ( ast -- ) char>> , ; inline

M: ast-identifier (compile) ( ast -- )
    name>> compile-identifier ; inline

M: ast-set (compile) ( ast -- )
    set>> , ; inline

M: ast-quotation (compile) ( ast -- )
    body>> [ [ (compile) ] each ] [ ] make , ; inline

M: ast-special (compile) ( ast -- ) value>> , ; inline

M: ast-boolean (compile) ( ast -- ) value>> , ; inline

: compiler-env ( -- )
    joy-env new default-env >>env
    H{ } clone >>user-env
    joy set ; inline

: @compile ( ast -- quot )
    [ [ (compile) ] each ] [ ] make ;

: compile ( ast -- quot )
    compiler-env @compile ;
