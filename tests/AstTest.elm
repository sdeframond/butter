module AstTest exposing (..)

import Document.AST exposing (..)
import Document.Types exposing (Value(..))
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, char, int, list, string, tuple)
import List as L
import String as S
import Test exposing (..)


suite : Test
suite =
    let
        testCollection f collection =
            L.map
                (\( input, output ) ->
                    test ("input: \"" ++ input ++ "\"")
                        (\_ -> Expect.equal (f input) output)
                )
                collection
    in
    describe "AST"
        -- TODO: add fuzzing when it will be possible to make advanced string fuzzers.
        -- See https://github.com/elm-explorations/test/issues/90
        [ describe "parseCell" <|
            testCollection parseCell
                [ ( "", Ok <| RootLiteral (StringValue "") )
                , ( "foo", Ok <| RootLiteral (StringValue "foo") )
                , ( "=1", Ok <| Formula (Literal (IntValue 1)) )
                , ( "=-1", Ok <| Formula (Literal (IntValue -1)) )
                , ( "=\"a\"", Ok <| Formula (Literal (StringValue "a")) )
                , ( "=foo123", Ok <| Formula (RelativeReference "foo123") )
                , ( "=foo123.bar123", Ok <| Formula (AbsoluteReference "foo123" "bar123") )
                , ( "=1+2", Ok <| Formula (BinOp PlusOp (Literal (IntValue 1)) (Literal (IntValue 2))) )

                -- There is a weird case where the `int` parser fails on the letter 'E'.
                , ( "=E1", Ok <| Formula (RelativeReference "E1") )
                ]
        , describe "parseName" <|
            testCollection parseName
                [ ( "foo123", Ok "foo123" )

                --, ( "foo bar", Err (Error "foo bar" [ { col = 4, problem = ExpectingEnd, row = 1 } ]) )
                ]
        ]
