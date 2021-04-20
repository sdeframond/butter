module AstTest exposing (..)

import Ast
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
            { resolveGlobalReference = \_ -> Ok (IntValue 1337)
            , resolveLocalReference = \_ -> Ok (IntValue 1338)
            }

        evalString : String -> Types.ValueOrError
        evalString input =
            Ast.parseCell input
                |> Result.mapError (always Types.ParsingError)
                |> Result.andThen (Ast.eval mockContext)

        toString : (String -> Maybe String) -> String -> Maybe String
        toString tag input =
            Ast.parseCell input
                |> Result.toMaybe
                |> Maybe.map (Ast.mapSheetReferences tag)
                |> Maybe.andThen Ast.toString
    in
    describe "AST"
        -- TODO: add fuzzing when it will be possible to make advanced string fuzzers.
        -- See https://github.com/elm-explorations/test/issues/90
        [ describe "parseCell & eval" <|
            testCollection evalString
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
            testCollection Ast.parseName
                [ ( "valid name", "foo123", Ok "foo123" )

                --, ( "invalid name", "foo bar", Err (Error "foo bar" [ { col = 4, problem = ExpectingEnd, row = 1 } ]) )
                ]
        , describe "parseCell & toString" <|
            testCollection (toString Just)
                [ ( "empty", "", Just "" )
                , ( "foo", "foo", Just "foo" )
                , ( "=1", "=1", Just "=1" )
                , ( "=-1", "=-1", Just "=-1" )
                , ( "string", "=\"a\"", Just "=\"a\"" )
                , ( "simple addition", "=1 + 2", Just "=1+2" )
                , ( "local ref", "=foo123", Just "=foo123" )
                , ( "absolute ref to existing value", "=foo123.bar123", Just "=foo123.bar123" )
                ]
                ++ testCollection (toString (always Nothing))
                    [ ( "absolute ref to undefined sheet", "=foo123.bar123", Nothing )
                    ]
        , todo "mapSheetReferences"
        ]
