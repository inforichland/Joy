! Copyright (C) 2009 Tim Wawrzynczak
! See http://factorcode.org/license.txt for BSD license.
USING: kernel namespaces sequences accessors
joy.ast joy.parser joy.pprint vectors
combinators math assocs math.ranges random 
quotations prettyprint math.functions 
calendar math.order macros generalizations fry 
parser words stack-checker strings io.encodings.utf8 
io io.files destructors arrays io.directories
continuations lists locals math.parser sequences.deep
joy.cons make effects ;

IN: joy.eval

TUPLE: joy-env env user-env dstack rstack ;

SYMBOL: joy

GENERIC: (@eval) ( ast -- )

GENERIC: (@compile) ( ast -- )

! ********************************************
! words for dealing with the joy environment
! ********************************************

: dstack-empty? ( -- ? )
    joy get dstack>> car +nil+ = ; inline

: dstack-push ( value -- )
    joy get [ cons ] change-dstack drop ; inline

: dstack-pop* ( -- value )
    joy get [ [ car ] [ cdr ] bi ] change-dstack drop ; inline
    
: dstack-pop ( -- )
    dstack-pop* drop ; inline

! ******************************
! compiling quotations
! ******************************

: compile-identifier ( ident -- )
    dup joy get env>> at
    [ % drop ]
    [
        dup joy get user-env>> at
        [ % drop ]
        [ ": Unknown word!" append throw ]
        if*
    ]
    if* ; inline

: @compile ( jc -- quot )
    [ [ (@compile) ] leach ] [ ] make ; inline

: do-push ( obj -- ) [ dstack-push ] curry % ; inline

M: object (@compile) ( obj -- ) do-push ; inline

M: ast-string (@compile) ( ast -- ) string>> do-push ; inline

M: ast-number (@compile) ( ast -- ) num>> do-push ; inline

M: ast-character (@compile) ( ast -- ) char>> do-push ; inline

M: ast-identifier (@compile) ( ast -- )
    name>> compile-identifier ; inline

M: ast-set (@compile) ( ast -- ) set>> do-push ; inline

M: joy-cons (@compile) ( ast -- )
    [ [ (@compile) ] leach ] [ ] make do-push ; inline

M: ast-boolean (@compile) ( ast -- ) value>> do-push ; inline

! ******************************
! utilities
! ******************************

: joy-call ( jc -- )
    dup +nil+ equal?
    [
        dup compiled-quot>> 
        [ dup @compile >>compiled-quot ] unless
        compiled-quot>> call
    ] unless ; inline

: joy-keep ( x quot -- x )
    over '[ _ joy-call ] call dip ; inline

! ******************************
! evaluation
! ******************************

: user-eval ( user-word -- )
    [ (@eval) ] each ; inline recursive

: eval-identifier ( identifier -- )
    {
        { [ dup joy get env>>      at ] [ joy get env>> at call ] }
        { [ dup joy get user-env>> at ] [ joy get user-env>> at user-eval ] }
        [ 2drop "Invalid word!" throw ]
    } cond ; inline recursive

: add-word-to-user-env ( quot name -- )
    joy get user-env>> set-at ;

: make-quot ( seq -- cons )
    [ +nil+ ] dip reverse
    [ dup ast-quotation? [ body>> make-quot ] when swap joy-cons ] each ; inline recursive

! ******************************
! generic eval word
! ******************************

M: object (@eval) ( obj -- ) dstack-push ; inline recursive

M: ast-definitions (@eval) ( ast -- )
    definitions>> [ ast-definition? ] deep-filter [ (@eval) ] each ; inline recursive

M: ast-definition (@eval) ( ast -- )
    [ body>> ] [ name>> ] bi add-word-to-user-env ; inline recursive

M: ast-string (@eval) ( ast -- )
    string>> dstack-push ; inline recursive

M: ast-number (@eval) ( ast -- )
    num>> dstack-push ; inline recursive

M: ast-character (@eval) ( ast -- )
    char>> dstack-push ; inline recursive

M: ast-identifier (@eval) ( ast -- )
    name>> eval-identifier ; inline recursive

M: ast-quotation (@eval) ( ast -- )
    body>> make-quot dstack-push ; inline recursive

M: ast-special (@eval) ( ast -- )
    value>> eval-identifier ; inline recursive

M: ast-boolean (@eval) ( ast -- )
    value>> dstack-push ; inline recursive

! *************************************
! helper functions
! *************************************

: unop ( quot -- )
    dstack-pop*
    swap call
    dstack-push ; inline

: binop ( quot -- )
    dstack-pop*
    dstack-pop* swap
    rot call
    dstack-push ; inline

! *************************************
! stack shuffling words
! *************************************

: dup-joy ( -- )
    dstack-pop* dup
    dstack-push
    dstack-push ; inline

: swap-joy ( -- )
    dstack-pop* dstack-pop*
    swap
    dstack-push dstack-push ; inline

: (dip-joy) ( quot -- )
    dstack-pop* ! pop TOS
    [ call ] dip
    dstack-push ; inline ! push back on to TOS

: dip-joy ( -- )
    dstack-pop*
    {
        { [ dup joy-cons?  ] [ '[ _ joy-call ] (dip-joy) ] }
        { [ dup quotation? ] [ (dip-joy) ] }
        [ drop "Not a quotation!" throw ]
    } cond ; inline

: rollup-joy ( -- ) ! X Y Z -- Z X Y (1 2 3 -- 3 1 2)
    dstack-pop* dstack-pop* dstack-pop* ! z y x (3 2 1)
    [ swap ] dip swap ! 2 1 3
    dstack-push dstack-push dstack-push ; inline

: rolldown-joy ( -- ) ! X Y Z -- Y Z X (1 2 3 -- 2 3 1)
    dstack-pop* dstack-pop* dstack-pop* ! z y x (3 2 1)
    swap [ swap ] dip ! 1 3 2
    dstack-push dstack-push dstack-push ; inline

: rotate-joy ( -- ) ! X Y Z -- Z Y X (1 2 3 -- 3 2 1)
    dstack-pop* dstack-pop* dstack-pop* ! z y x (3 2 1)
    [ swap ] dip swap [ swap ] dip
    dstack-push dstack-push dstack-push ; inline

: dupd-joy ( -- ) dstack-pop* dup-joy dstack-push ; inline

: swapd-joy ( -- ) dstack-pop* swap-joy dstack-push ; inline

: rollupd-joy ( -- ) dstack-pop* rollup-joy dstack-push ; inline

: rolldownd-joy ( -- ) dstack-pop* rolldown-joy dstack-push ; inline

: rotated-joy ( -- ) dstack-pop* rotate-joy dstack-push ; inline

: pop-joy ( -- ) dstack-pop ; inline

: popd-joy ( -- ) dstack-pop* pop-joy dstack-push ; inline

: print-joy ( -- ) dstack-pop* pprint " " write ; inline

! ***************************
! logic words
! ***************************

: or-joy ( -- )
    dstack-pop* dstack-pop* 
    {
        { [ dup boolean? ] [ or dstack-push ] }
        { [ dup number? ] [ [ 0 = ] bi@ and not dstack-push ] }
        [ 2drop "Invalid operands for 'or'!" throw ]
    } cond ; inline

: and-joy ( -- )
    dstack-pop* dstack-pop*
    {
        { [ dup boolean? ] [ and dstack-push ] }
        { [ dup number? ] [ [ 0 = ] bi@ or not dstack-push ] }
        [ 2drop "Invalid operands for 'and'!" throw ]
    } cond ; inline

: xor-joy ( -- )
    dstack-pop* dstack-pop*
    {
        { [ dup boolean? ] [ xor dstack-push ] }
        { [ dup number? ] [ = not dstack-push ] }
        [ 2drop "Invalid operands for 'xor'!" throw ]
    } cond ; inline 

! *****************************
! miscellaneous words
! *****************************

: false-joy ( -- ) f dstack-push ; inline
: true-joy  ( -- ) t dstack-push ; inline

: rand-joy ( -- ) 1 32767 [a,b] random dstack-push ; inline

: id-joy ( -- ) ; inline

: time-joy ( -- )
    now
    1970 1 1 0 0 0 0 hours <timestamp>
    time- duration>seconds floor dstack-push ; inline

! ****************************
! unary operations
! ****************************

: (sign) ( n -- n' )
    {
        { [ dup 0 > ] [ drop 1 ] }
        { [ dup 0 < ] [ drop -1 ] }
        [ drop 0 ]
    } cond ; inline
    
: sign-joy ( -- ) [ (sign) ] unop ; inline
: neg-joy ( -- ) [ 0 swap - ] unop ; inline
: abs-joy ( -- ) [ abs ] unop ; inline

! **************************
! trigish functions
! **************************

: cos-joy ( -- ) [ cos ] unop ; inline
: sin-joy ( -- ) [ sin ] unop ; inline
: tan-joy ( -- ) [ tan ] unop ; inline
: acos-joy ( -- ) [ acos ] unop ; inline
: asin-joy ( -- ) [ asin ] unop ; inline
: atan-joy ( -- ) [ atan ] unop ; inline
: cosh-joy ( -- ) [ cosh ] unop ; inline
: sinh-joy ( -- ) [ sinh ] unop ; inline
: tanh-joy ( -- ) [ tanh ] unop ; inline
: log-joy ( -- ) [ log ] unop ; inline
: log10-joy ( -- ) [ log 10 log / ] unop ; inline ! change of base formula
: pow-joy ( -- ) [ ^ ] binop ; inline
: sqrt-joy ( -- ) [ sqrt ] unop ; inline

! *******************************
! list (quotation) operations
! ******************************
: cons-joy ( -- ) [ joy-cons ] binop ; inline
: swons-joy ( -- ) [ swap joy-cons ] binop ; inline
: first-joy ( -- ) [ car ] unop ; inline
: rest-joy ( -- ) [ cdr ] unop ; inline
: of-joy ( -- ) [ swap [ cdr ] times car ] binop ; inline
: at-joy ( -- ) [ [ cdr ] times car ] binop ; inline
: size-joy ( -- ) [ llength ] unop ; inline
: uncons-joy ( -- ) dstack-pop* [ cdr ] [ car ] bi (@eval) dstack-push ; inline    
: drop-joy ( -- ) [ [ cdr ] times ] binop ; inline
: take-joy ( -- ) [ head ] binop ; inline
: concat-joy ( -- ) [ append ] binop ; inline
: enconcat-joy ( -- )
    dstack-pop* dstack-pop* dstack-pop*
    [ swap ] dip swap prefix append
    dstack-push ; inline
: null-joy ( -- ) ! empty aggregate or zero (0)
    dstack-pop*
    {
        { [ dup +nil+ =   ] [ dstack-push ] }
        { [ dup array?    ] [ length 0 = dstack-push ] }
        { [ dup joy-cons? ] [ nil? dstack-push ] }
        { [ dup number?   ] [ 0 = dstack-push ] }
        [ drop "Invalid operands for 'null'!" throw ]
    } cond ; inline
: small-joy ( -- ) ! 0/1 elements, or numeric one/zero
    dstack-pop*
    {
        { [ dup +nil+ =   ] [ drop t dstack-push ] }
        { [ dup array?    ] [ length [ 0 = ] [ 1 = ] bi or dstack-push ] }
        { [ dup joy-cons? ] [ [ car nil? ] [ cadr nil? ] bi or dstack-push ] }
        { [ dup number?   ] [ [ 0 = ] [ 1 = ] bi or dstack-push ] }
        [ drop "Invalid operands for 'small'!" throw ]
    } cond ; inline
: has-joy ( -- )
    dstack-pop* dstack-pop* ! thing seq --
    list>array member? dstack-push ; inline
: in-joy ( -- )
    dstack-pop* dstack-pop* list>array swap ! seq thing
    member? dstack-push ; inline

! ***************************************
! relational operators
! ***************************************
: >=-joy ( -- ) [ >= ] binop ; inline
: <=-joy ( -- ) [ <= ] binop ; inline
: <-joy ( -- ) [ < ] binop ; inline
: >-joy ( -- ) [ > ] binop ; inline
: !=-joy ( -- ) [ = not ] binop ; inline
: =-joy ( -- ) [ = ] binop ; inline    

! ***************************************
! math operations
! ***************************************
: +-joy ( -- ) [ + ] binop ; inline
: --joy ( -- ) [ - ] binop ; inline
: *-joy ( -- ) [ * ] binop ; inline
: /-joy ( -- ) [ / ] binop ; inline
: rem-joy ( -- ) [ mod ] binop ; inline
: div-joy ( -- ) [ /mod ] binop dstack-push ; inline
: ceil-joy ( -- ) [ ceiling ] unop ; inline
: floor-joy ( -- ) [ floor ] unop ; inline
: exp-joy ( -- ) [ exp ] unop ; inline
: trunc-joy ( -- ) [ truncate ] unop ; inline
: pred-joy ( -- ) [ 1 - ] unop ; inline
: succ-joy ( -- ) [ 1 + ] unop ; inline
: max-joy ( -- ) [ max ] binop ; inline
: min-joy ( -- ) [ min ] binop ; inline

! ***************************************
! predicate words
! ***************************************
: integer-joy ( -- ) [ integer? ] unop ; inline
: char-joy ( -- ) [ [ string? ] [ length 1 = ] bi and ] unop ; inline
: logical-joy ( -- ) [ boolean? ] unop ; inline
: set-joy ( -- ) [ array? ] unop ; inline
: string-joy ( -- ) [ string? ] unop ; inline
: list-joy ( -- ) [ joy-cons? ] unop ; inline
: leaf-joy ( -- ) [ joy-cons? not ] unop ; inline
! : user
: float-joy ( -- ) [ float? ] unop ; inline
! : file

! ***************************************
! file words
! ***************************************
: (fopen) ( mode pathname -- stream )
    dup string? [ "Not a filename!" throw ] unless swap ! pathname mode
    {
        { [ "r" over start ] [ drop utf8 <file-reader> ] }
        { [ "w" over start ] [ drop utf8 <file-writer> ] }
        { [ "a" over start ] [ drop utf8 <file-appender> ] }
        [ drop "You must specify a valid file mode!" throw ]
    } cond ;
: fopen-joy ( -- )
    dstack-pop* dstack-pop* 
    (fopen) dstack-push ; inline
: fflush-joy ( -- )
    dstack-pop* stream-flush ; inline
: fgetch-joy ( -- )
    dstack-pop* dup
    stream-read1 1array >string swap
    dstack-push dstack-push ; inline
: fgets-joy ( -- )
    dstack-pop* dup stream-readln swap
    dstack-push dstack-push ; inline
: fread-joy ( -- )
    dstack-pop* dstack-pop* ! n stream --
    dup [ stream-read ] dip swap ! data stream
    dstack-push dstack-push ; inline
: fwrite-joy ( -- )
    dstack-pop* dstack-pop* ! bytes stream --
    dup [ stream-write ] dip
    dstack-push ; inline
: fremove-joy ( -- )
    dstack-pop* [ delete-file true-joy ] [ 2drop false-joy ] recover ; inline
: frename-joy ( -- )
    dstack-pop* dstack-pop* swap
    [ move-file true-joy ] [ 3drop false-joy ] recover ; inline
: fputch-joy ( -- )
    dstack-pop* dstack-pop*
    dup [ stream-write1 ] dip
    dstack-push ; inline
: fputchars-joy ( -- ) fwrite-joy ; inline
: fclose-joy ( -- )
    dstack-pop* dispose ; inline

! **********************************
! combinators
! **********************************

: i-joy ( -- )
    dstack-pop* '[ _ joy-call ] call ; inline

: x-joy ( -- ) dup-joy i-joy ; inline

: preserving ( quot -- )
    joy get dstack>> clone joy get swap >>rstack drop ! quot --
    '[ _ joy-call ] call ! -- 
    joy get dstack>> lreverse [ joy get [ cons ] change-rstack ] leach
    joy get rstack>> clone joy get swap >>dstack
    +nil+ >>rstack drop ; inline

: ifte-joy ( -- )
    dstack-pop* '[ _ joy-call ] dstack-pop* '[ _ joy-call ] dstack-pop* ! f t pred
    preserving drop dstack-pop* spin if ; inline
    
: while-joy ( -- )
    dstack-pop* '[ _ joy-call ] dstack-pop* '[ _ joy-call ]
    [ joy-keep ] curry [ swap ] compose
    do compose [ loop ] curry when ; inline

: step-joy ( -- )
    dstack-pop* '[ _ joy-call ] dstack-pop* swap leach ; inline

: fold-joy ( -- )
    dstack-pop* dstack-pop* dstack-pop* spin
    reduce ; inline

: ((map-joy)) ( list quot -- cdr quot )
    [ [ car ] dip call ] [ [ cdr ] dip ] 2bi ; inline

: (map-joy) ( list quot -- )
    over nil? [ drop ] [ ((map-joy)) (map-joy) joy-cons ] if ; inline recursive

: map-joy ( -- )
    dstack-pop* '[ _ joy-call ] dstack-pop* swap (map-joy) ; inline

: times-joy ( -- )
    dstack-pop* '[ _ joy-call ] dstack-pop* swap times ; inline

:: (linrec) ( if-quot then-quot else1-quot else2-quot -- )
    if-quot preserving
    [ then-quot '[ _ joy-call ] call ]
    [ else1-quot '[ _ joy-call ] call
      if-quot then-quot else1-quot else2-quot (linrec)
      else2-quot call
    ] if ; inline recursive

: linrec-joy ( -- )
    dstack-pop* dstack-pop* dstack-pop* dstack-pop*
    4 -nrot spin
    (linrec) ; inline    
    
:: (tailrec) ( if-quot then-quot else-quot -- )
    if-quot preserving
    [ then-quot '[ _ joy-call ] call ]
    [ else-quot '[ _ joy-call ] call
      if-quot then-quot else-quot (tailrec)
    ] if ; inline recursive

: tailrec-joy ( -- )
    dstack-pop* dstack-pop* dstack-pop*
    spin (tailrec) ; inline

:: (binrec) ( if-quot then-quot prod-quot comb-quot -- )
    if-quot preserving
    [ then-quot '[ _ joy-call ] call ]
    [ prod-quot '[ _ joy-call ] call
      if-quot then-quot prod-quot comb-quot (binrec)
      [ if-quot then-quot prod-quot comb-quot (binrec) ] dip
      if-quot then-quot prod-quot comb-quot (binrec)
      comb-quot call
    ] if ; inline recursive

: binrec-joy ( -- )
    dstack-pop* dstack-pop* dstack-pop* dstack-pop*
    4 -nrot spin
    (binrec) ; inline

! **********************************
! printing words
! **********************************
: put-joy ( -- )
    dstack-pop*
    {
        { [ dup string? ] [ print ] }
        { [ dup number? ] [ number>string print ] }
    } cond ; inline

: putchars-joy ( -- )
    dstack-pop* write ; inline 

! **********************************
! regenerate the environment
! **********************************
: default-env ( -- env )
    H{
        { "+"     [ +-joy ] }
        { "-"     [ --joy ] }
        { "*"     [ *-joy ] }
        { "/"     [ /-joy ] }
        { "rand"  [ rand-joy ] }
        { "time"  [ time-joy ] }
        { "true"  [ true-joy ] }
        { "false" [ false-joy ] }
        
        { "dup"       [ dup-joy ] }
        { "swap"      [ swap-joy ] }
        { "dip"       [ dip-joy ] }
        { "pop"       [ pop-joy ] }
        { "."         [ print-joy ] }
        { "rollup"    [ rollup-joy ] }
        { "rolldown"  [ rolldown-joy ] }
        { "rotate"    [ rotate-joy ] }
        { "dupd"      [ dupd-joy ] }
        { "swapd"     [ swapd-joy ] }
        { "rollupd"   [ rollupd-joy ] }
        { "rolldownd" [ rolldownd-joy ] }
        { "rotated"   [ rotated-joy ] }
        { "popd"      [ popd-joy ] }
        { "id"        [ id-joy ] }
        
        { "or"  [ or-joy ] }
        { "and" [ and-joy ] }
        { "xor" [ xor-joy ] }
        
        { "rem"   [ rem-joy ] }
        { "div"   [ div-joy ] }
        { "sign"  [ sign-joy ] }
        { "neg"   [ neg-joy ] }
        { "ceil"  [ ceil-joy ] }
        { "floor" [ floor-joy ] }
        { "abs"   [ abs-joy ] }
        { "exp"   [ exp-joy ] }
        { "trunc" [ trunc-joy ] } 
        { "pred"  [ pred-joy ] }
        { "succ"  [ succ-joy ] }
        { "max"   [ max-joy ] }
        { "min"   [ min-joy ] }
        
        { "cos"   [ cos-joy ] }
        { "sin"   [ sin-joy ] }
        { "tan"   [ tan-joy ] }
        { "cosh"  [ cosh-joy ] }
        { "sinh"  [ sinh-joy ] }
        { "tanh"  [ tanh-joy ] }
        { "acos"  [ acos-joy ] }
        { "asin"  [ asin-joy ] }
        { "atan"  [ atan-joy ] }
        { "log"   [ log-joy ] }
        { "log10" [ log10-joy ] }
        { "pow"   [ pow-joy ] }

        { "cons"     [ cons-joy ] }
        { "swons"    [ swons-joy ] }
        { "first"    [ first-joy ] }
        { "rest"     [ rest-joy ] }
        { "of"       [ of-joy ] }
        { "at"       [ at-joy ] }
        { "size"     [ size-joy ] }
        { "uncons"   [ uncons-joy ] }
        { "drop"     [ drop-joy ] }
        { "take"     [ take-joy ] }
        { "concat"   [ concat-joy ] }
        { "enconcat" [ enconcat-joy ] }
        { "null"     [ null-joy ] }
        { "small"    [ small-joy ] }
        { "in"       [ in-joy ] }
        { "has"      [ has-joy ] }

        { ">=" [ >=-joy ] }
        { "<=" [ <=-joy ] }
        { "<"  [ <-joy ] }
        { ">"  [ >-joy ] }
        { "="  [ =-joy ] }
        { "!=" [ !=-joy ] }

        { "integer" [ integer-joy ] }
        { "char"    [ char-joy ] }
        { "logical" [ logical-joy ] }
        { "string"  [ string-joy ] }
        { "list"    [ list-joy ] }
        { "leaf"    [ leaf-joy ] }
        { "float"   [ float-joy ] }

        { "fopen"   [ fopen-joy ] }
        { "fclose"  [ fclose-joy ] }
        { "fflush"  [ fflush-joy ] }
        { "fgetch"  [ fgetch-joy ] }
        { "fgets"   [ fgets-joy ] }
        { "fread"   [ fread-joy ] }
        { "fwrite"  [ fwrite-joy ] }
        { "fremove" [ fremove-joy ] }
        { "frename" [ frename-joy ] }
        { "fputch"  [ fputch-joy ] }

        { "i"       [ i-joy ] }
        { "x"       [ x-joy ] }
        { "ifte"    [ ifte-joy ] }
        { "while"   [ while-joy ] }
        { "step"    [ step-joy ] }
        { "map"     [ map-joy ] }
        { "fold"    [ fold-joy ] }
        { "times"   [ times-joy ] }
        { "linrec"  [ linrec-joy ] }
        { "tailrec" [ tailrec-joy ] }
        { "binrec"  [ binrec-joy ] }
      !  { "case"    [ case-joy ] }

        { "put"      [ put-joy ] }
        { "putchars" [ putchars-joy ] }
        { "print"    [ print-joy ] }
      !  { "."        [ print-joy ] }
      !  { "putch"    [ putch-joy ] }
        
    } ;

! **********************************
! actual eval
! **********************************
: eval-env ( -- )
    joy-env new default-env >>env
    +nil+ >>dstack
    +nil+ >>rstack
    H{ } clone >>user-env
    joy set ;

: (eval) ( string -- )
    parse-joy
    [ (@eval) ] each ; inline

! now clean up the stack, in case any
! raw ast- tuples are left on it
! (from being removed from a quotation)
: cleanup ( -- )
    joy get dstack>> clone joy get swap >>rstack
    +nil+ >>dstack
    rstack>> lreverse
    [ (@eval) ] leach
    joy get +nil+ >>rstack drop ; inline

: eval ( string -- )
    eval-env ! new environment
    (eval) ! evaluate
    cleanup ;
