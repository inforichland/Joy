! Copyright (C) 2009 Tim Wawrzynczak
! See http://factorcode.org/license.txt for BSD license.
USING: kernel arrays strings math ;
IN: joy.ast

! abstract syntax tree for joy

TUPLE: ast-quotation      { body array } ;
TUPLE: ast-definition     { name string } { body array } ;
TUPLE: ast-definitions    definitions ;
TUPLE: ast-string         { string string } ;
TUPLE: ast-character      { char string } ;
TUPLE: ast-number         { num number } ;
TUPLE: ast-identifier     { name string } ;
TUPLE: ast-special        { value string } ;
TUPLE: ast-module-keyword { value string } ;
TUPLE: ast-set            { set array } ;
TUPLE: ast-boolean        { value boolean } ;
