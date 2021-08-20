module FormulaTest exposing (..)

import Expect
import Formula
import Helpers exposing (testCollection)
import Name exposing (Name)
import PositiveInt
import Test exposing (..)
import Types exposing (Value(..))


suite : Test
suite =
    let
        mockContext : Formula.Context Types.SheetId
        mockContext =
            { resolveGlobalReference = \_ _ -> Ok (IntValue 1337)
            , resolveLocalReference = \_ _ -> Ok (IntValue 1338)
            , prefix = PositiveInt.one
            , ancestors = []
            }

        alwaysId1 =
            always (Just PositiveInt.one)

        evalString : String -> Types.ValueOrError
        evalString input =
            Formula.fromSource alwaysId1 input
                |> Formula.eval mockContext

        sourceView : (Name -> Maybe Types.SheetId) -> String -> Maybe String
        sourceView getSheetId =
            Formula.fromSource getSheetId
                >> Formula.sourceView (always (Just (Name.unsafeFromString "name")))
    in
    describe "Formula"
        [ -- TODO: add fuzzing when it will be possible to make advanced string fuzzers.
          -- See https://github.com/elm-explorations/test/issues/90
          testCollection "eval"
            evalString
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
                    Formula.fromSource alwaysId1 "1 + 1"
                        |> Formula.initialInput
                        -- instead of "1+1"
                        |> Expect.equal "1 + 1"
            , test "return the unmodified user input when invalid" <|
                \_ ->
                    Formula.fromSource alwaysId1 "foo bar baz"
                        |> Formula.initialInput
                        |> Expect.equal "foo bar baz"
            ]
        , testCollection "sourceView when id found"
            (sourceView alwaysId1)
            [ ( "empty", "", Just "" )
            , ( "integer", "1", Just "1" )
            , ( "negative integer", "-1", Just "-1" )
            , ( "string", "\"a\"", Just "\"a\"" )
            , ( "simple addition", "1 + 2", Just "1+2" )
            , ( "local ref", "foo123", Just "foo123" )
            , ( "absolute ref to existing value", "foo123.bar123", Just "name.bar123" )
            ]
        , testCollection "sourceView when id not found"
            (sourceView (always Nothing))
            [ ( "fallback on user input when a reference is not defined", "foo123.bar123", Just "foo123.bar123" )
            ]
        , describe "isValid"
            [ test "when valid" <|
                \_ ->
                    Formula.fromSource alwaysId1 "thisisvalid"
                        |> Formula.isValid
                        |> Expect.true "expected isValid to return True"
            , test "when not valid" <|
                \_ ->
                    Formula.fromSource alwaysId1 "this is not valid"
                        |> Formula.isValid
                        |> Expect.false "expected isValid to return False"
            ]
        , testCollection "parseInt when valid"
            Formula.parseInt
            [ ( "positive integer", "32", Ok 32 )
            , ( "negative integer", "-32", Ok -32 )
            , ( "with leading space", "   32", Ok 32 )
            , ( "with trailing space", "32   ", Ok 32 )
            , ( "with spaces between minus and digits", " -   32 ", Ok -32 )
            ]
        , testCollection "parseInt when not valid"
            (Formula.parseInt >> Result.mapError (always Nothing))
            [ ( "empty string", "", Err Nothing )
            , ( "not a digit", "foo", Err Nothing )
            , ( "trailing alpha", "123foo", Err Nothing )
            ]
        ]
