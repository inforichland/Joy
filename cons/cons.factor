! Copyright (C) 2009 Tim Wawrzynczak
! See http://factorcode.org/license.txt for BSD license.
USING: kernel lists accessors ;

IN: joy.cons

TUPLE: joy-cons { car read-only } { cdr read-only } compiled-quot ;

: joy-cons ( car cdr -- cons ) f \ joy-cons boa ; inline

INSTANCE: joy-cons list

M: joy-cons car ( jc -- car ) car>> ;
M: joy-cons cdr ( jc -- car ) cdr>> ;
M: joy-cons nil? ( jc -- ? ) drop f ;
