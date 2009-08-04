! Copyright (C) 2009 Tim Wawrzynczak
! See http://factorcode.org/license.txt for BSD license.
USING: kernel namespaces sequences accessors
joy.ast joy.parser joy.pprint vectors
combinators math assocs math.ranges random 
quotations prettyprint math.functions 
calendar math.order macros generalizations fry 
parser words stack-checker ;

IN: joy.eval

TUPLE: joy-env env { dstack vector } { rstack vector } ;

SYMBOL: joy

! 
! utilities
! 

! macro borrowed from Slava

MACRO: preserving ( quot -- )
    [ infer in>> length ] keep '[ _ ndup @ ] ;

: ifte ( pred t f -- )
    [ preserving ] 2dip if ; inline

! words for dealing with the joy environment

: default-env ( -- env )
    [ ] H{ } make-assoc ;

: dstack-empty? ( -- ? )
    joy get dstack>> empty? ;

: dstack-push ( value -- )
    joy get dstack>> push ;

: dstack-pop* ( -- value )
    joy get dstack>> pop ;

: dstack-pop ( -- )
    dstack-pop* drop ; inline

! evaluation

: eval-identifier ( identifier -- )
    joy get env>> at
    [ call ] [ "Invalid word!" throw ] if* ; inline

: add-word-to-env ( [quot-name] -- )
    [ first ] [ second ] bi
    joy get env>> set-at ;

! generic eval word

GENERIC: (@eval) ( ast -- )

M: ast-string (@eval) ( ast -- )
    string>> dstack-push ; inline

M: ast-number (@eval) ( ast -- )
    num>> dstack-push ; inline

M: ast-character (@eval) ( ast -- )
    char>> dstack-push ; inline

M: ast-identifier (@eval) ( ast -- )
    name>> eval-identifier ; inline

M: ast-quotation (@eval) ( ast -- )
    body>> >quotation dstack-push ; inline

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

! trig functions

: cos-joy ( -- ) [ cos ] unop ; inline
: sin-joy ( -- ) [ sin ] unop ; inline
: tan-joy ( -- ) [ tan ] unop ; inline
: acos-joy ( -- ) [ acos ] unop ; inline
: asin-joy ( -- ) [ asin ] unop ; inline
: atan-joy ( -- ) [ atan ] unop ; inline
: cosh-joy ( -- ) [ cosh ] unop ; inline
: sinh-joy ( -- ) [ sinh ] unop ; inline
: tanh-joy ( -- ) [ tanh ] unop ; inline

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

! regenerate the environment
: (env) ( -- )
    joy-env new default-env >>env
    V{ } clone >>dstack
    V{ } clone >>rstack
    joy set ;

: env ( -- )
    (env)
    ! add words to the environment
    {
        { [ +-joy ] "+" }
        { [ --joy ] "-" }
        { [ *-joy ] "*" }
        { [ /-joy ] "/" }
        { [ rand-joy ] "rand" }
        { [ time-joy ] "time" }

        { [ dup-joy ] "dup" }
        { [ swap-joy ] "swap" }
        { [ dip-joy ] "dip" }
        { [ pop-joy ] "pop" }
        { [ print-joy ] "." }
        { [ rollup-joy ] "rollup" }
        { [ rolldown-joy ] "rolldown" }
        { [ rotate-joy ] "rotate" }
        { [ dupd-joy ] "dupd" }
        { [ swapd-joy ] "swapd" }
        { [ rollupd-joy ] "rollupd" }
        { [ rolldownd-joy ] "rolldownd" }
        { [ rotated-joy ] "rotated" }
        { [ popd-joy ] "popd" }
        { [ id-joy ] "id" }

        { [ or-joy ] "or" }
        { [ and-joy ] "and" }
        { [ xor-joy ] "xor" }

        { [ rem-joy ] "rem" }
        { [ div-joy ] "div" }
        { [ sign-joy ] "sign" }
        { [ neg-joy ] "neg" }
        { [ ceil-joy ] "ceil" }
        { [ floor-joy ] "floor" }
        { [ abs-joy ] "abs" }
        { [ exp-joy ] "exp" }
        { [ trunc-joy ] "trunc" }
        { [ pred-joy ] "pred" }
        { [ succ-joy ] "succ" }
        { [ max-joy ] "max" }
        { [ min-joy ] "min" }

        { [ cos-joy ] "cos" }
        { [ sin-joy ] "sin" }
        { [ tan-joy ] "tan" }
        { [ cosh-joy ] "cosh" }
        { [ sinh-joy ] "sinh" }
        { [ tanh-joy ] "tanh" }
        { [ acos-joy ] "acos" }
        { [ asin-joy ] "asin" }
        { [ atan-joy ] "atan" }
        
    } [ add-word-to-env ] each ;

! actual eval 

: (eval) ( string -- )
    parse-joy
    [ (@eval) ] each ;

: eval ( string -- )
    env ! new environment
    (eval) ;
