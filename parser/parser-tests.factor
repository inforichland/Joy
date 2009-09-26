! Copyright (C) 2009 Tim Wawrzynczak
! See http://factorcode.org/license.txt for BSD license.
USING: tools.test joy.parser joy.ast ;
IN: joy.parser.tests

! numbers / identifiers
[
    V{
        T{ ast-number { num 1 } }
        T{ ast-number { num 2 } }
        T{ ast-number { num 3 } }
        T{ ast-identifier { name "*" } }
        T{ ast-identifier { name "+" } }
    }
] [ "1 2 3 * +" parse-joy ] unit-test

! nested quotations
[
    V{
        T{ ast-number { num 1 } }
        T{ ast-quotation
           { body
             {
                 T{ ast-number { num 2 } }
                 T{ ast-quotation
                    { body
                      {
                          T{ ast-number { num 3 } }
                          T{ ast-quotation
                             { body
                               {
                                   T{ ast-number
                                      { num 4 }
                                   }
                                   T{ ast-quotation
                                      { body
                                        {
                                            T{
                                                ast-number
                                                { num
                                                  5
                                                }
                                            }
                                        }
                                      }
                                   }
                                   T{ ast-identifier
                                      { name "dip" }
                                   }
                               }
                             }
                          }
                          T{ ast-identifier { name "dip" } }
                      }
                    }
                 }
                 T{ ast-identifier { name "dip" } }
             }
           }
        }
        T{ ast-identifier { name "dip" } }
    }
] [ "1 [2 [3 [4 [5] dip] dip] dip] dip] dip" parse-joy ] unit-test

! definitions
[
    V{
        T{ ast-definitions
           { definitions
             V{
                 T{ ast-definition
                    { name "foo" }
                    { body
                      {
                          T{ ast-number { num 1 } }
                          T{ ast-number { num 2 } }
                      }
                    }
                 }
                 T{ ast-definition
                    { name "baz" }
                    { body
                      { T{ ast-identifier { name "i" } } }
                    }
                 }
                 T{ ast-definition
                    { name "quux" }
                    { body
                      {
                          T{ ast-identifier { name "foo" } }
                          T{ ast-quotation
                             { body
                               {
                                   T{ ast-identifier
                                      { name "dup" }
                                   }
                               }
                             }
                          }
                          T{ ast-identifier { name "baz" } }
                      }
                    }
                 }
             }
           }
        }
    }
] [ "DEFINE foo == 1 2; baz == i; quux == foo [dup] baz." parse-joy ] unit-test

! comments
[
    V{ }
] [ "(* this is really cool 1 2 * + >= DEFINE foo == i. *)" parse-joy ] unit-test

! numbers
[
    V{
        T{ ast-set
           { set
             {
                 T{ ast-number { num 1 } }
                 T{ ast-number { num 2 } }
                 T{ ast-number { num 3 } }
                 T{ ast-number { num 4 } }
             }
           }
        }
        T{ ast-identifier { name "dup" } }
    }
] [ "{ 1 2 3 4} dup" parse-joy ] unit-test

! relationals
[
    V{
        T{ ast-number { num 1 } }
        T{ ast-number { num 2 } }
        T{ ast-identifier { name ">=" } }
        T{ ast-number { num 3 } }
        T{ ast-identifier { name "<=" } }
        T{ ast-number { num 2 } }
        T{ ast-identifier { name "<" } }
        T{ ast-number { num 4 } }
        T{ ast-identifier { name ">" } }
    }
] [ "1 2 >= 3 <= 2 < 4 >" parse-joy ] unit-test

! LIBRA
[
    V{
        T{ ast-definitions
           { definitions
             {
                 T{ ast-definition
                    { name "stuff" }
                    { body
                      {
                          T{ ast-string
                             { string
                               "factor is concatenative"
                             }
                          }
                      }
                    }
                 }
             }
           }
        }
        T{ ast-identifier { name "stuff" } }
        T{ ast-identifier { name "putchars" } }
    }
] [ "LIBRA stuff == \"factor is concatenative\". stuff putchars" parse-joy ] unit-test

! character literals
[
    V{
        T{ ast-character { char "X" } }
        T{ ast-character { char "Y" } }
        T{ ast-character { char "Z" } }
        T{ ast-string
           { string "next time won't you sing with me" }
        }
    }
] [ "'X 'Y 'Z \"next time won't you sing with me\"" parse-joy ] unit-test

! misc
[ 123 ] [ V{ 49 50 51 } check-number ] unit-test

! check-number
[ V{ 1 2 3 } check-number ] [ invalid-number? ] must-fail-with

! make sure the hyphen is correctly parsed as part of the word
[ V{ T{ ast-identifier { name "foo-bar" } } } ] [ "foo-bar" parse-joy ] unit-test
