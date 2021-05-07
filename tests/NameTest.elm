module NameTest exposing (..)

import Expect
import Helpers exposing (testCollection)
import Name
import PositiveInt
import Test exposing (..)


suite : Test
suite =
    describe "Name"
        [ describe "fromString & toString" <|
            [ describe "when valid" <|
                testCollection (Name.fromString >> Maybe.map Name.toString)
                    [ ( "simple name", "foo", Just "foo" )
                    , ( "with trailing digits", "foo123", Just "foo123" )
                    , ( "with leading space", "   foo", Just "foo" )
                    , ( "with trailing space", "foo   ", Just "foo" )
                    ]
            , describe "when not valid" <|
                testCollection Name.fromString
                    [ ( "empty string", "", Nothing )
                    , ( "starts with a digit", "123foo", Nothing )
                    , ( "contains a space", "foo bar", Nothing )
                    , ( "contains a dot", "foo.bar", Nothing )
                    ]
            ]
        , test "fromSheetId produces a valid name" <|
            \_ ->
                Name.fromSheetId PositiveInt.one
                    |> Name.toString
                    |> Name.fromString
                    |> Maybe.map Name.toString
                    |> Expect.equal (Just "Sheet1")
        , test "fromCoord produces a valid name" <|
            \_ ->
                Name.fromCoord PositiveInt.one PositiveInt.one
                    |> Name.toString
                    |> Name.fromString
                    |> Maybe.map Name.toString
                    |> Expect.equal (Just "A1")
        ]
