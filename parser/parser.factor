! Copyright (C) 2009 Tim Wawrzynczak
! See http://factorcode.org/license.txt for BSD license.
USING: kernel peg peg.ebnf strings arrays unicode.categories
sequences sequences.deep math math.parser parser-combinators
lists math.parser accessors joy.ast sets strings.parser ;
IN: joy.parser

ERROR: invalid-number str ;

: check-number ( seq -- n )
    >string dup string>number [ ] [ invalid-number ] ?if ;

: make-string ( seq -- str )
    [ dup fixnum? [ 1string ] when ] map
    "" [ append ] reduce ;

EBNF: parse-joy

Character = .
WhitespaceCharacter = (" " | "\t" | "\r" | "\n")
DecimalDigit = [0-9]
Letter = [A-Za-z]

CommentCharacter = [^*]
Comment = "(*" (CommentCharacter)*:s "*)" => [[ ignore ]]

OptionalWhitespace = (WhitespaceCharacter | Comment)*
Whitespace = (WhitespaceCharacter | Comment)+

LetterOrDigit = DecimalDigit | Letter
Identifier = (Letter | "_"):h (LetterOrDigit | "_" | "-")*:t => [[ { h t } flatten make-string ]]
IdentifierLiteral = Identifier:i => [[ i ast-identifier boa ]]

OptionalMinus = ("-" => [[ t ]])?
IntegerLiteral = (OptionalMinus:m UnsignedIntegerLiteral:i) => [[ i m [ neg ] when ast-number boa ]]
UnsignedIntegerLiteral = DecimalDigit+:d => [[ d check-number ]]

QuotationElement = AnyLiteral
QuotationLiteral = "[" OptionalWhitespace
                   (QuotationElement:h
                    (Whitespace QuotationElement:e => [[ e ]])*:t
                    => [[ t h prefix ]]
                    )?:elts OptionalWhitespace "]" => [[ elts >array ast-quotation boa ]]

SetElement = AnyLiteral
SetLiteral = "{" OptionalWhitespace
             (SetElement:h
             (Whitespace SetElement:e => [[ e ]])*:t
             => [[ t h prefix ]]
             )?:elts OptionalWhitespace "}" => [[ elts prune >array ast-set boa ]]
                    
CharacterLiteral = "'" Character:c => [[ c 1string ast-character boa ]]

StringLiteral = '"' (StringLiteralCharacter | '\"' => [[ CHAR: " ]])*:s '"' => [[ s >string unescape-string ast-string boa ]]
StringLiteralCharacter = [^"]

Boolean = "true" | "false"
BooleanLiteral = Boolean:b => [[ b >string "true" = ast-boolean boa ]]

Relational = ">=" | ">" | "<=" | "<" | "=" | "!="
RelationalLiteral = Relational:r => [[ r >string ast-identifier boa ]]

BuiltinIdentifierLiteral = ("+" | "-" | "*" | "/"):i => [[ i >string ast-identifier boa ]]

Definition = Identifier:i OptionalWhitespace "==" (Expression)+:j
                                              => [[ i j >array ast-definition boa ]]

DefineWord = "DEFINE" | "LIBRA"
Defines = DefineWord Whitespace (Definition OptionalWhitespace ";" OptionalWhitespace)*:d
                              Definition:e OptionalWhitespace "."
                        => [[ d dup empty? [ drop e 1array ] [ e suffix ] if [ ast-definition? ] deep-filter ast-definitions boa ]]
                                              
AnyLiteral = IdentifierLiteral |
             IntegerLiteral |
             RelationalLiteral |
             BooleanLiteral |
             BuiltinIdentifierLiteral |
             QuotationLiteral |
             SetLiteral |
             StringLiteral |
             CharacterLiteral
                                              
Expression = OptionalWhitespace
             AnyLiteral:e => [[ e ]]

Code = OptionalWhitespace ( (Comment | Defines | Expression):e OptionalWhitespace => [[ e ]])*:h OptionalWhitespace => [[ h ]]

;EBNF
         
