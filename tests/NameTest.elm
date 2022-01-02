module NameTest exposing (..)

import Core.Name as Name
import Core.PositiveInt as PositiveInt
import Expect
import Fuzz
import Helpers exposing (testCollection)
import Test exposing (..)


expectToMatchString : String -> Name.Name -> Expect.Expectation
expectToMatchString string =
    Name.toString
        >> Name.fromString
        >> Maybe.map Name.toString
        >> Expect.equal (Just string)


expectValidName : Name.Name -> Expect.Expectation
expectValidName =
    Name.toString
        >> Name.fromString
        >> Maybe.map Name.toString
        >> Expect.notEqual Nothing


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
                Name.appendInt (Name.sanitize "name") 24525
                    |> expectToMatchString "name24525"
        , test "fromCoord produces a valid name" <|
            \_ ->
                Name.fromCoord PositiveInt.one PositiveInt.one
                    |> expectToMatchString "A1"
        , fuzz Fuzz.string "sanitize always produces valid names" (Name.sanitize >> expectValidName)
        , test "sanitize with a valid string returns a name that matches it" <|
            \_ ->
                Name.sanitize "aValidString"
                    |> expectToMatchString "aValidString"
        ]
