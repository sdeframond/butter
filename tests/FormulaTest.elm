module FormulaTest exposing (..)

import Expect
import Formula
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

        mockContext : Formula.Context Types.SheetId
        mockContext =
            { resolveGlobalReference = \_ _ -> Ok (IntValue 1337)
            , resolveLocalReference = \_ _ -> Ok (IntValue 1338)
            , prefix = 0
            , ancestors = []
            }

        alwaysId123 =
            -- String.filter Char.isDigit >> String.toInt
            always (Just 123)

        evalString : String -> Types.ValueOrError
        evalString input =
            Formula.fromSource alwaysId123 input
                |> Formula.eval mockContext

        sourceView : (String -> Maybe Types.SheetId) -> String -> Maybe String
        sourceView getSheetId =
            Formula.fromSource getSheetId
                >> Formula.sourceView (always (Just "someref"))
    in
    describe "Formula"
        [ -- TODO: add fuzzing when it will be possible to make advanced string fuzzers.
          -- See https://github.com/elm-explorations/test/issues/90
          describe "eval" <|
            testCollection evalString
                [ ( "empty", "", Err Types.ParsingError )
                , ( "invalid input 1", "this is not valid", Err Types.ParsingError )
                , ( "integer", "1", Ok (IntValue 1) )
                , ( "negative integer", "-1", Ok (IntValue -1) )
                , ( "string", "\"a\"", Ok (StringValue "a") )
                , ( "local ref", "foo123", Ok (IntValue 1338) )
                , ( "trailing space after ref", "thisisvalid ", Ok (IntValue 1338) )
                , ( "absolute ref", "foo123.bar123", Ok (IntValue 1337) )
                , ( "simple addition", "1+2", Ok (IntValue 3) )

                -- There is a weird case where the `int` parser fails on the letter 'E'.
                , ( "special case E1", "E1", Ok (IntValue 1338) )
                ]
        , describe "initialInput"
            [ test "return the unmodified user input when valid" <|
                \_ ->
                    Formula.fromSource alwaysId123 "1 + 1"
                        |> Formula.initialInput
                        -- instead of "1+1"
                        |> Expect.equal "1 + 1"
            , test "return the unmodified user input when invalid" <|
                \_ ->
                    Formula.fromSource alwaysId123 "foo bar baz"
                        |> Formula.initialInput
                        |> Expect.equal "foo bar baz"
            ]
        , describe "sourceView" <|
            testCollection (sourceView alwaysId123)
                [ ( "empty", "", Just "" )
                , ( "integer", "1", Just "1" )
                , ( "negative integer", "-1", Just "-1" )
                , ( "string", "\"a\"", Just "\"a\"" )
                , ( "simple addition", "1 + 2", Just "1+2" )
                , ( "local ref", "foo123", Just "foo123" )
                , ( "absolute ref to existing value", "foo123.bar123", Just "someref.bar123" )
                ]
                ++ testCollection (sourceView (always Nothing))
                    [ ( "fallback on user input when a reference is not defined", "foo123.bar123", Just "foo123.bar123" )
                    ]
        , describe "isValid"
            [ test "when valid" <|
                \_ ->
                    Formula.fromSource alwaysId123 "thisisvalid"
                        |> Formula.isValid
                        |> Expect.true "expected isValid to return True"
            , test "when not valid" <|
                \_ ->
                    Formula.fromSource alwaysId123 "this is not valid"
                        |> Formula.isValid
                        |> Expect.false "expected isValid to return False"
            ]
        , describe "parseInt" <|
            [ describe "when valid" <|
                testCollection Formula.parseInt
                    [ ( "positive integer", "32", Ok 32 )
                    , ( "negative integer", "-32", Ok -32 )
                    , ( "with leading space", "   32", Ok 32 )
                    , ( "with trailing space", "32   ", Ok 32 )
                    , ( "with spaces between minus and digits", " -   32 ", Ok -32 )
                    ]
            , describe "when not valid" <|
                testCollection (Formula.parseInt >> Result.mapError (always Nothing))
                    [ ( "empty string", "", Err Nothing )
                    , ( "not a digit", "foo", Err Nothing )
                    , ( "trailing alpha", "123foo", Err Nothing )
                    ]
            ]
        , describe "parseName" <|
            [ describe "when valid" <|
                testCollection Formula.parseName
                    [ ( "simple name", "foo", Ok "foo" )
                    , ( "with trailing digits", "foo123", Ok "foo123" )
                    , ( "with leading space", "   foo", Ok "foo" )
                    , ( "with trailing space", "foo   ", Ok "foo" )
                    ]
            , describe "when not valid" <|
                testCollection (Formula.parseName >> Result.mapError (always Nothing))
                    [ ( "empty string", "", Err Nothing )
                    , ( "starts with a digit", "123foo", Err Nothing )
                    , ( "contains a space", "foo bar", Err Nothing )
                    , ( "contains a dot", "foo.bar", Err Nothing )
                    ]
            ]
        ]