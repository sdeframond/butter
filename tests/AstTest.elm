module AstTest exposing (..)

import AST
import Expect
import List as L
import Test exposing (..)
import Types exposing (Value(..))


suite : Test
suite =
    let
        testCollection f collection =
            L.map
                (\( caseName, input, output ) ->
                    test caseName
                        (\_ -> Expect.equal (f input) output)
                )
                collection

        mockContext =
            { resolveAbsolute = \_ -> Ok (IntValue 1337)
            , resolveRelative = \_ -> Ok (IntValue 1338)
            }
    in
    describe "AST"
        -- TODO: add fuzzing when it will be possible to make advanced string fuzzers.
        -- See https://github.com/elm-explorations/test/issues/90
        [ describe "evalString" <|
            testCollection (AST.evalString mockContext)
                [ ( "empty", "", Ok (StringValue "") )
                , ( "foo", "foo", Ok (StringValue "foo") )
                , ( "=1", "=1", Ok (IntValue 1) )
                , ( "=-1", "=-1", Ok (IntValue -1) )
                , ( "string", "=\"a\"", Ok (StringValue "a") )
                , ( "local ref", "=foo123", Ok (IntValue 1338) )
                , ( "absolute ref", "=foo123.bar123", Ok (IntValue 1337) )
                , ( "simple addition", "=1+2", Ok (IntValue 3) )

                -- There is a weird case where the `int` parser fails on the letter 'E'.
                , ( "local ref, special case E1", "=E1", Ok (IntValue 1338) )
                ]
        , describe "parseName" <|
            testCollection AST.parseName
                [ ( "valid name", "foo123", Ok "foo123" )

                --, ( "invalid name", "foo bar", Err (Error "foo bar" [ { col = 4, problem = ExpectingEnd, row = 1 } ]) )
                ]
        , describe "updateReferences" <|
            testCollection (AST.updateReferences (always "bar")) <|
                [ ( "root literal", "foo", Ok "foo" )
                , ( "local reference", "=foo", Ok "=foo" )
                , ( "global reference", "=foo.foo", Ok "=bar.foo" )
                ]
        ]
