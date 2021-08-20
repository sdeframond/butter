module NameTest exposing (..)

import Expect
import Helpers exposing (testCollection)
import Name
import PositiveInt
import Test exposing (..)


suite : Test
suite =
    describe "Name"
        [ testCollection "fromString & toString when valid"
            (Name.fromString >> Maybe.map Name.toString)
            [ ( "simple name", "foo", Just "foo" )
            , ( "with trailing digits", "foo123", Just "foo123" )
            , ( "with leading space", "   foo", Just "foo" )
            , ( "with trailing space", "foo   ", Just "foo" )
            ]
        , testCollection "fromString & toString when not valid"
            Name.fromString
            [ ( "empty string", "", Nothing )
            , ( "starts with a digit", "123foo", Nothing )
            , ( "contains a space", "foo bar", Nothing )
            , ( "contains a dot", "foo.bar", Nothing )
            ]
        , test "appendInt produces a valid name" <|
            \_ ->
                Name.appendInt (Name.unsafeFromString "name") 24525
                    |> Name.toString
                    |> Name.fromString
                    |> Maybe.map Name.toString
                    |> Expect.equal (Just "name24525")
        , test "fromCoord produces a valid name" <|
            \_ ->
                Name.fromCoord PositiveInt.one PositiveInt.one
                    |> Name.toString
                    |> Name.fromString
                    |> Maybe.map Name.toString
                    |> Expect.equal (Just "A1")
        ]
