! Copyright (C) 2009 Tim Wawrzynczak
! See http://factorcode.org/license.txt for BSD license.
USING: kernel namespaces sequences accessors
joy.ast joy.parser joy.pprint vectors
combinators math assocs math.ranges random 
quotations prettyprint math.functions 
calendar math.order macros generalizations fry 
parser words stack-checker strings ;

IN: joy.eval

TUPLE: joy-env env user-env { dstack vector } { rstack vector } { quotation-depth fixnum } ;

SYMBOL: joy

GENERIC: (@eval) ( ast -- )

! 
! utilities
! 

! macro borrowed from Slava

MACRO: preserving ( quot -- )
    [ infer in>> length ] keep '[ _ ndup @ ] ;

: ifte ( pred t f -- )
    [ preserving ] 2dip if ; inline

! words for dealing with the joy environment

: dstack-empty? ( -- ? )
    joy get dstack>> empty? ;

: dstack-push ( value -- )
    joy get dstack>> push ;

: dstack-pop* ( -- value )
    joy get dstack>> pop ;

: dstack-pop ( -- )
    dstack-pop* drop ; inline

! evaluation

: user-eval ( user-word -- )
    [ (@eval) ] each ;

: eval-identifier ( identifier -- )
    {
        { [ dup joy get env>> at ]      [ joy get env>> at call ] }
        { [ dup joy get user-env>> at ] [ joy get user-env>> at user-eval ] }
        [ 2drop "Invalid word!" throw ]
    } cond ; inline

: add-word-to-user-env ( name quot -- )
    joy get user-env>> set-at ;

: incr-quotation-depth ( -- ) joy get dup quotation-depth>> 1 + >>quotation-depth joy set ;
: decr-quotation-depth ( -- ) joy get dup quotation-depth>> 1 - >>quotation-depth joy set ;
: get-quotation-depth ( -- n ) joy get quotation-depth>> ; inline

! generic eval word

M: ast-definitions (@eval) ( ast -- )
    definitions>> [ ast-definition? ] filter [ (@eval) ] each ; inline

M: ast-definition (@eval) ( ast -- )
    [ body>> ] [ name>> ] bi add-word-to-user-env ; inline

M: ast-string (@eval) ( ast -- )
    string>> dstack-push ; inline

M: ast-number (@eval) ( ast -- )
    num>> dstack-push ; inline

M: ast-character (@eval) ( ast -- )
    char>> dstack-push ; inline

M: ast-identifier (@eval) ( ast -- )
    get-quotation-depth 0 =
    [ name>> eval-identifier ] when ; inline

M: ast-quotation (@eval) ( ast -- )
    incr-quotation-depth
    body>> >quotation dstack-push
    decr-quotation-depth ; inline

M: ast-special (@eval) ( ast -- )
    value>> eval-identifier ; inline

M: ast-boolean (@eval) ( ast -- )
    value>> dstack-push ; inline

!
! helper functions
!

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
! words you can use
! *************************************

! combinators
: (i-joy) ( quot -- )
    [ (@eval) ] each ; inline 

: i-joy ( -- )
    dstack-pop* dup
    quotation?
    [ (i-joy) ] [ drop "Not a quotation!" throw ] if ; inline

! stack shuffling words

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
    [ [ (@eval) ] each ] dip
    dstack-push ; inline ! push back on to TOS

: dip-joy ( -- )
    dstack-pop* dup
    quotation?
    [ (dip-joy) ] [ drop "Not a quotation!" throw ] if ; inline

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

: print-joy ( -- ) dstack-pop* pprint ; inline

! logic words

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

! miscellaneous words

: false-joy ( -- ) f dstack-push ; inline
: true-joy  ( -- ) t dstack-push ; inline

: rand-joy ( -- ) 1 32767 [a,b] random dstack-push ; inline

: id-joy ( -- ) ; inline

: time-joy ( -- )
    now
    1970 1 1 0 0 0 0 hours <timestamp>
    time- duration>seconds floor dstack-push ; inline

! unary operations

: (sign) ( n -- n' )
    {
        { [ dup 0 > ] [ drop 1 ] }
        { [ dup 0 < ] [ drop -1 ] }
        [ drop 0 ]
    } cond ; inline
    
: sign-joy ( -- ) [ (sign) ] unop ; inline
: neg-joy ( -- ) [ 0 swap - ] unop ; inline
: abs-joy ( -- ) [ abs ] unop ; inline

! trigish functions

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

! list (quotation) operations
: cons-joy ( -- ) [ swap prefix ] binop ; inline
: swons-joy ( -- ) [ prefix ] binop ; inline
: first-joy ( -- ) [ first ] unop ; inline
: rest-joy ( -- ) [ rest ] unop ; inline
: of-joy ( -- ) [ nth ] binop ; inline
: at-joy ( -- ) [ swap nth ] binop ; inline
: size-joy ( -- ) [ length ] unop ; inline
: uncons-joy ( -- ) [ dup rest [ first ] dip ] unop dstack-push ; inline
: drop-joy ( -- ) [ tail ] binop ; inline
: take-joy ( -- ) [ head ] binop ; inline
: concat-joy ( -- ) [ append ] binop ; inline
: enconcat-joy ( -- )
    dstack-pop* dstack-pop* dstack-pop*
    [ swap ] dip swap prefix append
    dstack-push ; inline
: null-joy ( -- ) ! empty aggregate or zero (0)
    dstack-pop*
    {
        { [ dup quotation? ] [ empty? dstack-push ] }
        { [ dup number? ] [ 0 = dstack-push ] }
        [ drop "Invalid operands for 'null'!" throw ]
    } cond ; inline
: small-joy ( -- ) ! 0/1 elements, or numeric one/zero
    dstack-pop*
    {
        { [ dup quotation? ] [ length [ 0 = ] [ 1 = ] bi or dstack-push ] }
        { [ dup number? ] [ [ 0 = ] [ 1 = ] bi or dstack-push ] }
        [ drop "Invalid operands for 'small'!" throw ]
    } cond ; inline
: has-joy ( -- )
    dstack-pop* dstack-pop* ! thing seq --
    member? dstack-push ; inline
: in-joy ( -- )
    dstack-pop* dstack-pop*
    member? dstack-push ; inline

! relational operators
: >=-joy ( -- ) [ >= ] binop ; inline
: <=-joy ( -- ) [ <= ] binop ; inline
: <-joy ( -- ) [ < ] binop ; inline
: >-joy ( -- ) [ > ] binop ; inline
: !=-joy ( -- ) [ = not ] binop ; inline
: =-joy ( -- ) [ = ] binop ; inline    

! binary operations

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

! predicate words
: integer-joy ( -- ) [ number? ] unop ; inline
: char-joy ( -- ) [ [ string? ] [ length 1 = ] bi and ] unop ; inline
: logical-joy ( -- ) [ boolean? ] unop ; inline
! : set-joy ( -- )  :TODO:
: string-joy ( -- ) [ string? ] unop ; inline
: list-joy ( -- ) [ quotation? ] unop ; inline
: leaf-joy ( -- ) [ quotation? not ] unop ; inline
! : user
! : float
! : file

! regenerate the environment
: default-env ( -- env )
    H{
        { "+"    [ +-joy ] }
        { "-"    [ --joy ] }
        { "*"    [ *-joy ] }
        { "/"    [ /-joy ] }
        { "rand" [ rand-joy ] }
        { "time" [ time-joy ] }
        
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
        { "i"        [ i-joy ] }
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
        { "<" [ <-joy ] }
        { ">" [ >-joy ] }
        { "=" [ =-joy ] }
        { "!=" [ !=-joy ] }

        { "integer" [ integer-joy ] }
        { "char-joy" [ char-joy ] }
        { "logical" [ logical-joy ] }
        { "string" [ string-joy ] }
        { "list" [ list-joy ] }
        { "leaf" [ leaf-joy ] }
        
    } ;
        
: env ( -- )
    joy-env new default-env >>env
    V{ } clone >>dstack
    V{ } clone >>rstack
    H{ } clone >>user-env
    0 >>quotation-depth
    joy set ;

! actual eval 

: (eval) ( string -- )
    parse-joy
    [ (@eval) ] each ;

: eval ( string -- )
    env ! new environment
    (eval) ;
